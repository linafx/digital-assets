// Copyright (c) 2020 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.lf.speedy

/**
  * The simplified AST for the speedy interpreter.
  *
  * This reduces the number of binding forms by moving update and scenario
  * expressions into builtins.
  */
import java.util

import com.daml.lf.language.Ast._
import com.daml.lf.data.Ref._
import com.daml.lf.value.{Value => V}
import com.daml.lf.speedy.SValue._
import com.daml.lf.speedy.Speedy._
import com.daml.lf.speedy.SError._
import com.daml.lf.speedy.SBuiltin._
import com.daml.lf.speedy.SResult._
import java.util.ArrayList

/** The speedy expression:
  * - de Bruijn indexed.
  * - closure converted.
  * - multi-argument applications and abstractions.
  * - all update and scenario operations converted to builtin functions.
  */
sealed abstract class SExpr extends Product with Serializable {
  def execute(machine: Machine): Unit
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  override def toString: String =
    productPrefix + productIterator.map(SExpr.prettyPrint).mkString("(", ",", ")")
}

@SuppressWarnings(Array("org.wartremover.warts.Any"))
object SExpr {

  /** Expression variants which correspond to evaluator continuations...
    Named with a leading K, instead of a leading SE */

  /** The function has been evaluated to a value, now start evaluating the arguments. */
  final case class KArg(args: Array[SExpr]) extends SExpr with SomeArrayEquals {
    def execute(machine: Machine): Unit = {
      val func = machine.popEnv()
      executeApplication(func,args,machine)
    }
  }

  /** Collect an evaluted argument; and begin evaluation of the next */
  final case class KCollectArg(args: util.ArrayList[SValue], next: SExpr) extends SExpr {
    def execute(machine: Machine): Unit = {
      args.add(machine.popEnv())
      machine.ctrl = next
    }
  }

  /** The function and all arguments have been evaluated. Enter the function */
  final case class KFun(prim: Prim, args: util.ArrayList[SValue], arity: Int) extends SExpr {
    private val kpop = prim match {
      case PClosure(_, vars) => KPop(arity + vars.size)
      case PBuiltin(_) => null
    }
    def execute(machine: Machine): Unit = {
      args.add(machine.popEnv())
      machine.enterFullyAppliedFunction(kpop, prim, args)
    }
  }

  /** The function and some arguments have been evaluated. Construct a PAP from them. */
  final case class KPAP(prim: Prim, args: util.ArrayList[SValue], arity: Int) extends SExpr {
    def execute(machine: Machine): Unit = {
      args.add(machine.popEnv())
      machine.returnValue(SPAP(prim, args, arity))
    }
  }

  /** The scrutinee of a match has been evaluated, now match the alternatives against it. */
  final case class KMatch(alts: Array[SCaseAlt]) extends SExpr with SomeArrayEquals {
    def execute(machine: Machine): Unit = {
      val value = machine.popEnv()
      matchAlternative(alts, value, machine)
    }
  }

  /** Pop 'count' arguments from the environment. */
  final case class KPop(count: Int) extends SExpr {
    def execute(machine: Machine) = {
      val value = machine.popEnv()
      machine.popEnvN(count)
      machine.returnValue(value)
    }
  }

  /** A location frame stores a location annotation found in the AST. */
  final case class KLocationTrace(location: Location) extends SExpr {
    def execute(machine: Machine) = {
      val value = machine.popEnv()
      machine.returnValue(value)
    }
  }

  /** A catch frame marks the point to which an exception (of type 'SErrorDamlException')
    * is unwound. The 'envSize' specifies the size to which the environment must be pruned.
    * If an exception is raised and 'KCatch' is found from kont-stack, then 'handler' is
    * evaluated. If 'KCatch' is encountered naturally, then 'fin' is evaluated.
    */
  final case class KCatchMarker(handler: SExpr, fin: SExpr, envSize: Int) extends SExpr {
    def execute(machine: Machine) = {
      val _ = machine.popEnv()
      machine.ctrl = fin
    }
  }

  /** Store the evaluated value in the 'SEVal' from which the expression came from.
    * This in principle makes top-level values lazy. It is a useful optimization to
    * allow creation of large constants (for example records) that are repeatedly
    * accessed. In older compilers which did not use the builtin record and struct
    * updates this solves the blow-up which would happen when a large record is
    * updated multiple times. */
  final case class KCacheVal(eval: SEVal, stack_trace: List[Location]) extends SExpr {
    def execute(machine: Machine): Unit = {
      val value = machine.popEnv()
      machine.pushStackTrace(stack_trace)
      eval.setCached(value, stack_trace)
      machine.returnValue(value)
    }
  }

  /** Finished; machine has computed final value */
  final case class KFinished() extends SExpr {
    def execute(machine: Machine): Unit = {
      val value = machine.popEnv()
      throw SpeedyHungry(SResultFinalValue(value))
    }
  }

  /** Expression variants which correspond to syntactic forms in DAML... */

  /** Reference to a variable. 'index' is the 1-based de Bruijn index,
    * that is, SEVar(1) points to the top-most value in the environment.
    * https://en.wikipedia.org/wiki/De_Bruijn_index
    */
  final case class SEVar(index: Int) extends SExpr {
    def execute(machine: Machine): Unit = {
      machine.returnValue(machine.getEnv(index))
    }
  }

  /** Reference to a value. On first lookup the evaluated expression is
    * stored in 'cached'.
    */
  final case class SEVal(
      ref: SDefinitionRef,
  ) extends SExpr {

    // The variable `_cached` is used to cache the evaluation of the
    // LF value defined by `ref` once it has been computed.  Hence we
    // avoid both the lookup in the package definition HashMap and the
    // full reevaluation of the body of the definition.
    // Here we take advantage of the Java memory model
    // (https://docs.oracle.com/javase/specs/jls/se8/html/jls-17.html#jls-17.7)
    // that guarantees the write of `_cache` (in the method
    // `setCached`) is done atomically.
    // This is similar how hashcode evaluation is cached in String
    // http://hg.openjdk.java.net/jdk8/jdk8/jdk/file/tip/src/share/classes/java/lang/String.java
    private var _cached: Option[(SValue, List[Location])] = None

    def cached: Option[(SValue, List[Location])] = _cached

    def setCached(sValue: SValue, stack_trace: List[Location]): Unit =
      _cached = Some((sValue, stack_trace))

    def execute(machine: Machine): Unit =
      machine.lookupVal(this)
  }

  /** Reference to a builtin function */
  final case class SEBuiltin(b: SBuiltin) extends SExpr {
    def execute(machine: Machine): Unit = {
      /* special case for nullary record constructors */
      b match {
        case SBRecCon(id, fields) if b.arity == 0 =>
          machine.returnValue(SRecord(id, fields, new ArrayList()))
        case _ =>
          machine.returnValue(SPAP(PBuiltin(b), new util.ArrayList[SValue](), b.arity))
      }
    }
  }

  /** A pre-computed value, usually primitive literal, e.g. integer, text, boolean etc. */
  final case class SEValue(v: SValue) extends SExpr {
    def execute(machine: Machine): Unit = {
      machine.returnValue(v)
    }
  }

  object SEValue extends SValueContainer[SEValue]

  /** Function application. Apply 'args' to function 'fun', where 'fun'
    * evaluates to a builtin or a closure.
    */
  final case class SEApp(fun: SExpr, args: Array[SExpr]) extends SExpr with SomeArrayEquals {
    private val k = KArg(args)
    def execute(machine: Machine): Unit = {
      machine.pushKont(k)
      machine.ctrl = fun
    }
  }

  /** Lambda abstraction. Transformed into SEMakeClo in lambda lifting.
    * NOTE(JM): Compilation done in two passes so that closure conversion
    * can be written against this simplified expression type.
    */
  final case class SEAbs(arity: Int, body: SExpr) extends SExpr {
    def execute(machine: Machine): Unit =
      crash("unexpected SEAbs, expected SEMakeClo")
  }

  object SEAbs {
    // Helper for constructing abstraction expressions:
    // SEAbs(1) { ... }
    def apply(arity: Int)(body: SExpr): SExpr = SEAbs(arity, body)

    val identity: SEAbs = SEAbs(1, SEVar(1))
  }

  /** Closure creation. Create a new closure object storing the free variables
    * in 'body'.
    */
  final case class SEMakeClo(fv: Array[Int], arity: Int, body: SExpr)
      extends SExpr
      with SomeArrayEquals {

    def execute(machine: Machine): Unit = {
      def convertToSValues(fv: Array[Int], getEnv: Int => SValue) = {
        val sValues = new Array[SValue](fv.length)
        var i = 0
        while (i < fv.length) {
          sValues(i) = getEnv(fv(i))
          i = i + 1
        }
        sValues
      }

      val sValues = convertToSValues(fv, machine.getEnv)
      machine.returnValue(SPAP(PClosure(body, sValues), new util.ArrayList[SValue](), arity))
    }
  }

  /** Pattern match. */
  final case class SECase(scrut: SExpr, alts: Array[SCaseAlt]) extends SExpr with SomeArrayEquals {
    private val k = KMatch(alts)
    def execute(machine: Machine): Unit = {
      machine.pushKont(k)
      machine.ctrl = scrut
    }

    override def toString: String = s"SECase($scrut, ${alts.mkString("[", ",", "]")})"
  }

  object SECase {

    // Helper for constructing case expressions:
    // SECase(scrut) of(
    //   SECaseAlt(SCPNil ,...),
    //   SECaseAlt(SCPDefault, ...),
    // )
    case class PartialSECase(scrut: SExpr) {
      def of(alts: SCaseAlt*): SExpr = SECase(scrut, alts.toArray)
    }

    def apply(scrut: SExpr) = PartialSECase(scrut)
  }

  /** A non-recursive, non-parallel let block. Each bound expression
    * is evaluated in turn and pushed into the environment one by one,
    * with later expressions possibly referring to earlier.
    */
  final case class SELet(bounds: Array[SExpr], body: SExpr) extends SExpr with SomeArrayEquals {
    private val k1 = KPop(bounds.size)
    def execute(machine: Machine): Unit = {
      // Pop the block once we're done evaluating the body
      machine.pushKont(k1)

      // Evaluate the body after we've evaluated the binders
      machine.pushKont(body)

      // Start evaluating the let binders
      for (i <- 1 until bounds.size) {
        val b = bounds(bounds.size - i)
        machine.pushKont(b)
      }
      machine.ctrl = bounds.head
    }
  }

  object SELet {

    // Helpers for constructing let expressions:
    // Instead of
    //   SELet(Array(SEVar(4), SEVar(1)), SEVar(1))
    // you can write:
    //   SELet(
    //     SEVar(4),
    //     SEVar(1)
    //   ) in SEVar(1)
    case class PartialSELet(bounds: Array[SExpr]) extends SomeArrayEquals {
      def in(body: => SExpr): SExpr = SELet(bounds, body)
    }

    def apply(bounds: SExpr*) = PartialSELet(bounds.toArray)

  }

  /** Location annotation. When encountered the location is stored in the 'lastLocation'
    * variable of the machine. When commit is begun the location is stored in 'commitLocation'.
    */
  final case class SELocation(loc: Location, expr: SExpr) extends SExpr {
    def execute(machine: Machine): Unit = {
      machine.pushLocation(loc)
      machine.ctrl = expr
    }
  }

  /** A catch expression. This is used internally solely for the purpose of implementing
    * mustFailAt. If the evaluation of 'body' causes an exception of type 'DamlException'
    * (see SError), then the environment and continuation stacks are reset and 'handler'
    * is executed. If the evaluation is successful, then the 'fin' expression is evaluated.
    * This is on purpose very limited, with no mechanism to inspect the exception, nor a way
    * to access the value returned from 'body'.
    */
  final case class SECatch(body: SExpr, handler: SExpr, fin: SExpr) extends SExpr {
    def execute(machine: Machine): Unit = {
      machine.pushKont(KCatchMarker(handler, fin, machine.env.size))
      machine.ctrl = body
    }
  }

  /** When we fetch a contract id from upstream we cannot crash in the upstream
    * calls. Rather, we set the control to this expression and then crash when executing.
    */
  final case class SEWronglyTypeContractId(
      acoid: V.AbsoluteContractId,
      expected: TypeConName,
      actual: TypeConName,
  ) extends SExpr {
    def execute(machine: Machine): Unit = {
      throw DamlEWronglyTypedContract(acoid, expected, actual)
    }
  }

  final case class SEImportValue(value: V[V.ContractId]) extends SExpr {
    def execute(machine: Machine): Unit = {
      machine.importValue(value)
    }
  }

  /** Case patterns */
  sealed trait SCasePat

  /** Match on a variant. On match the value is unboxed and pushed to environment. */
  final case class SCPVariant(id: Identifier, variant: Name, constructorRank: Int) extends SCasePat

  /** Match on a variant. On match the value is unboxed and pushed to environment. */
  final case class SCPEnum(id: Identifier, constructor: Name, constructorRank: Int) extends SCasePat

  /** Match on a primitive constructor, that is on true, false or unit. */
  final case class SCPPrimCon(pc: PrimCon) extends SCasePat

  /** Match on an empty list. */
  final case object SCPNil extends SCasePat

  /** Match on a list. On match, the head and tail of the list is pushed to environment. */
  final case object SCPCons extends SCasePat

  /** Default match case. Always matches. */
  final case object SCPDefault extends SCasePat

  final case object SCPNone extends SCasePat

  final case object SCPSome extends SCasePat

  /** Case alternative. If the 'pattern' matches, then the environment is accordingly
    * extended and 'body' is evaluated. */
  final case class SCaseAlt(pattern: SCasePat, body: SExpr)

  sealed abstract class SDefinitionRef {
    def ref: DefinitionRef
    def packageId: PackageId = ref.packageId
    def modName: ModuleName = ref.qualifiedName.module
  }

  // references to definitions that come from the archive
  final case class LfDefRef(ref: DefinitionRef) extends SDefinitionRef
  // references to definitions generated by the Speedy compiler
  final case class ChoiceDefRef(ref: DefinitionRef, choiceName: ChoiceName) extends SDefinitionRef

  //
  // List builtins (foldl, foldr, equalList) are implemented as recursive
  // definition to save java stack
  //

  final case class SEBuiltinRecursiveDefinition(ref: SEBuiltinRecursiveDefinition.Reference)
      extends SExpr {

    import SEBuiltinRecursiveDefinition._

    def execute(machine: Machine): Unit = {
      val body = ref match {
        case Reference.FoldL => foldLBody
        case Reference.FoldR => foldRBody
        case Reference.EqualList => equalListBody
      }
      body.execute(machine)
    }
  }

  final object SEBuiltinRecursiveDefinition {

    sealed abstract class Reference

    final object Reference {
      final case object FoldL extends Reference
      final case object FoldR extends Reference
      final case object EqualList extends Reference
    }

    val FoldL: SEBuiltinRecursiveDefinition = SEBuiltinRecursiveDefinition(Reference.FoldL)
    val FoldR: SEBuiltinRecursiveDefinition = SEBuiltinRecursiveDefinition(Reference.FoldR)
    val EqualList: SEBuiltinRecursiveDefinition = SEBuiltinRecursiveDefinition(Reference.EqualList)

    private val foldLBody: SExpr =
      // foldl f z xs =
      SEMakeClo(
        Array(),
        3,
        // case xs of
        SECase(SEVar(1)) of (
          // nil -> z
          SCaseAlt(SCPNil, SEVar(2)),
          // cons y ys ->
          SCaseAlt(
            SCPCons,
            // foldl f (f z y) ys
            SEApp(
              FoldL,
              Array(
                SEVar(5), /* f */
                SEApp(
                  SEVar(5),
                  Array(
                    SEVar(4), /* z */
                    SEVar(2) /* y */
                  )
                ),
                SEVar(1) /* ys */
              )
            )
          )
        )
      )

    private val foldRBody: SExpr =
      // foldr f z xs =
      SEMakeClo(
        Array(),
        3,
        // case xs of
        SECase(SEVar(1)) of (// nil -> z
        SCaseAlt(SCPNil, SEVar(2)),
        // cons y ys ->
        SCaseAlt(
          SCPCons,
          // f y (foldr f z ys)
          SEApp(
            SEVar(5),
            Array(
              /* f */
              SEVar(2), /* y */
              SEApp(
                FoldR,
                Array(
                  /* foldr f z ys */
                  SEVar(5), /* f */
                  SEVar(4), /* z */
                  SEVar(1) /* ys */
                )
              )
            )
          )
        ))
      )

    private val equalListBody: SExpr =
      // equalList f xs ys =
      SEMakeClo(
        Array(),
        3,
        // case xs of
        SECase(SEVar(2) /* xs */ ) of (
          // nil ->
          SCaseAlt(
            SCPNil,
            // case ys of
            //   nil -> True
            //   default -> False
            SECase(SEVar(1)) of (SCaseAlt(SCPNil, SEValue.True),
            SCaseAlt(SCPDefault, SEValue.False))
          ),
          // cons x xss ->
          SCaseAlt(
            SCPCons,
            // case ys of
            //       True -> listEqual f xss yss
            //       False -> False
            SECase(SEVar(3) /* ys */ ) of (
              // nil -> False
              SCaseAlt(SCPNil, SEValue.False),
              // cons y yss ->
              SCaseAlt(
                SCPCons,
                // case f x y of
                SECase(SEApp(SEVar(7), Array(SEVar(4), SEVar(2)))) of (
                  SCaseAlt(
                    SCPPrimCon(PCTrue),
                    SEApp(EqualList, Array(SEVar(7), SEVar(1), SEVar(3))),
                  ),
                  SCaseAlt(SCPPrimCon(PCFalse), SEValue.False)
                )
              )
            )
          )
        )
      )
  }

  private def prettyPrint(x: Any): String =
    x match {
      case i: Array[Any] => i.mkString("[", ",", "]")
      case i: Array[Int] => i.mkString("[", ",", "]")
      case i: java.util.ArrayList[_] => i.toArray().mkString("[", ",", "]")
      case other: Any => other.toString
    }

}
