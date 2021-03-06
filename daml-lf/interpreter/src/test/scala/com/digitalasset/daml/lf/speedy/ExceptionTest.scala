// Copyright (c) 2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.lf
package speedy

import com.daml.lf.PureCompiledPackages
import com.daml.lf.data
import com.daml.lf.language.Ast._
import com.daml.lf.value.Value._
import com.daml.lf.speedy.Compiler.FullStackTrace
import com.daml.lf.speedy.SResult.{SResultFinalValue, SResultError}
import com.daml.lf.speedy.SError.DamlEUnhandledException
import com.daml.lf.testing.parser.Implicits._
import com.daml.lf.validation.Validation
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ExceptionTest extends AnyWordSpec with Matchers with TableDrivenPropertyChecks {

  "builtin exception types: constructors & destructors" should {

    // Construction/Deconstruction of builtin-exception types are inverses.

    val pkgs: PureCompiledPackages = typeAndCompile(p"""
       module M {
         val text : Text = "Hello!";
         val t2 : Text = GENERAL_ERROR_MESSAGE (MAKE_GENERAL_ERROR M:text);
         val t3 : Text = ARITHMETIC_ERROR_MESSAGE (MAKE_ARITHMETIC_ERROR M:text);
         val t4 : Text = CONTRACT_ERROR_MESSAGE (MAKE_CONTRACT_ERROR M:text);
       }
      """)

    val testCases = Table[String, String](
      ("expression", "string-result"),
      ("M:text", "Hello!"),
      ("M:t2", "Hello!"),
      ("M:t3", "Hello!"),
      ("M:t4", "Hello!"),
    )

    forEvery(testCases) { (exp: String, res: String) =>
      s"""eval[$exp] --> "$res"""" in {
        val expected: SResult = SResultFinalValue(SValue.SText(res))
        runExpr(pkgs)(e"$exp") shouldBe expected
      }
    }
  }

  "builtin exceptions: to/from-any-exception" should {

    // Convertion to/from AnyException works as expected:
    // specifically: we can only extract the inner value if the
    // expected-type (on: from_any_exception) matches the actual-type (on: to_any_exception)

    val pkgs: PureCompiledPackages = typeAndCompile(p"""
       module M {
         val G : AnyException = to_any_exception @GeneralError (MAKE_GENERAL_ERROR "G");
         val A : AnyException = to_any_exception @ArithmeticError (MAKE_ARITHMETIC_ERROR "A");
         val C : AnyException = to_any_exception @ContractError (MAKE_CONTRACT_ERROR "C");
         val fromG : AnyException -> Text = \(e:AnyException) -> case from_any_exception @GeneralError e of None -> "NONE" | Some x -> GENERAL_ERROR_MESSAGE x;
         val fromA : AnyException -> Text = \(e:AnyException) -> case from_any_exception @ArithmeticError e of None -> "NONE" | Some x -> ARITHMETIC_ERROR_MESSAGE x;
         val fromC : AnyException -> Text = \(e:AnyException) -> case from_any_exception @ContractError e of None -> "NONE" | Some x -> CONTRACT_ERROR_MESSAGE x;
       }
      """)

    val testCases = Table[String, String](
      ("expression", "string-result"),
      ("M:fromG M:G", "G"),
      ("M:fromA M:A", "A"),
      ("M:fromC M:C", "C"),
      ("M:fromG M:A", "NONE"),
      ("M:fromG M:C", "NONE"),
      ("M:fromA M:G", "NONE"),
      ("M:fromA M:C", "NONE"),
      ("M:fromC M:G", "NONE"),
      ("M:fromC M:A", "NONE"),
    )

    forEvery(testCases) { (exp: String, res: String) =>
      s"""eval[$exp] --> "$res"""" in {
        val expected: SResult = SResultFinalValue(SValue.SText(res))
        runExpr(pkgs)(e"$exp") shouldBe expected
      }
    }
  }

  "ANY_EXCEPTION_MESSAGE" should {

    // Application of ANY_EXCEPTION_MESSAGE for various cases:
    // all 3 builtin exception types & 2 user-defined exceptions.
    // MyException1 contains the message text directly
    // MyException2 composes the message string from two text contained in the payload

    val pkgs: PureCompiledPackages = typeAndCompile(p"""
       module M {

         record @serializable MyException1 = { message: Text } ;
         exception MyException1 = {
           message \(e: M:MyException1) -> M:MyException1 {message} e
         };

        val e1 : AnyException = to_any_exception @M:MyException1 (M:MyException1 { message = "Message1" });
        val t1 : Text = ANY_EXCEPTION_MESSAGE M:e1;

         record @serializable MyException2 = { front: Text, back: Text } ;

         exception MyException2 = {
           message \(e: M:MyException2) -> APPEND_TEXT (M:MyException2 {front} e) (M:MyException2 {back} e)
         };

        val e2 : AnyException = to_any_exception @M:MyException2 (M:MyException2 { front = "Mess", back = "age2" });
        val t2 : Text = ANY_EXCEPTION_MESSAGE M:e2;

        val e3 : AnyException = to_any_exception @GeneralError (MAKE_GENERAL_ERROR "MessageG");
        val t3 : Text = ANY_EXCEPTION_MESSAGE M:e3;

        val e4 : AnyException = to_any_exception @ArithmeticError (MAKE_ARITHMETIC_ERROR "MessageA");
        val t4 : Text = ANY_EXCEPTION_MESSAGE M:e4;

        val e5 : AnyException = to_any_exception @ContractError (MAKE_CONTRACT_ERROR "MessageC");
        val t5 : Text = ANY_EXCEPTION_MESSAGE M:e5;
       }
      """)

    val testCases = Table[String, String](
      ("expression", "expected-string"),
      ("M:t1", "Message1"),
      ("M:t2", "Message2"),
      ("M:t3", "MessageG"),
      ("M:t4", "MessageA"),
      ("M:t5", "MessageC"),
    )

    forEvery(testCases) { (exp: String, res: String) =>
      s"""eval[$exp] --> "$res"""" in {
        val expected: SResult = SResultFinalValue(SValue.SText(res))
        runExpr(pkgs)(e"$exp") shouldBe expected
      }
    }
  }

  "unhandled throw" should {

    // Behaviour when no handler catches a throw:
    // 1: GeneralError thrown; no try-catch in scope
    // 2. GeneralError thrown; try catch in scope, but handler does not catch
    // 3. User Exception thrown; no handler in scope
    // 4. User Exception thrown; no handler in scope; secondary throw from the message function of the 1st exception

    val pkgs: PureCompiledPackages = typeAndCompile(p"""
       module M {

         val unhandled1 : Update Int64 =
            upure @Int64 (ADD_INT64 10 (throw @Int64 @GeneralError (MAKE_GENERAL_ERROR "oops1")));

         val unhandled2 : Update Int64 =
            try @Int64 (upure @Int64 (ADD_INT64 10 (throw @Int64 @GeneralError (MAKE_GENERAL_ERROR "oops2"))))
            catch e -> None @(Update Int64);

         record @serializable E1 = { } ;
         exception E1 = { message \(e: M:E1) -> "E1" };
         val unhandled3 : Update Int64 = upure @Int64 (throw @Int64 @M:E1 (M:E1 {})) ;

         record @serializable E2 = { } ;
         exception E2 = { message \(e: M:E2) -> throw @Text @M:E1 (M:E1 {}) } ; //throw from the message function
         val unhandled4 : Update Int64 = upure @Int64 (throw @Int64 @M:E2 (M:E2 {})) ;

       }
      """)

    val e1 =
      data.Ref.Identifier.assertFromString(s"${defaultParserParameters.defaultPackageId}:M:E1")
    val e2 =
      data.Ref.Identifier.assertFromString(s"${defaultParserParameters.defaultPackageId}:M:E2")

    val testCases = Table[String, SResult](
      ("expression", "expected"),
      (
        "M:unhandled1",
        SResultError(
          DamlEUnhandledException(
            TBuiltin(BTGeneralError),
            ValueBuiltinException("GeneralError", ValueText("oops1")),
          )
        ),
      ),
      (
        "M:unhandled2",
        SResultError(
          DamlEUnhandledException(
            TBuiltin(BTGeneralError),
            ValueBuiltinException("GeneralError", ValueText("oops2")),
          )
        ),
      ),
      (
        "M:unhandled3",
        SResultError(
          DamlEUnhandledException(TTyCon(e1), ValueRecord(Some(e1), data.ImmArray.empty))
        ),
      ),
      (
        "M:unhandled4",
        SResultError(
          DamlEUnhandledException(TTyCon(e2), ValueRecord(Some(e2), data.ImmArray.empty))
        ),
      ),
    )

    forEvery(testCases) { (exp: String, expected: SResult) =>
      s"eval[$exp] --> $expected" in {
        runUpdateExpr(pkgs)(e"$exp") shouldBe expected
      }
    }
  }

  "throw/catch (GeneralError)" should {

    // Basic throw/catch example for a builtin (GeneralError) exception

    val pkgs: PureCompiledPackages = typeAndCompile(p"""
       module M {
         val throwAndCatch : Update Int64 =
           try @Int64 (upure @Int64 (ADD_INT64 10 (throw @Int64 @GeneralError (MAKE_GENERAL_ERROR "oops"))))
           catch e -> Some @(Update Int64) (upure @Int64 77);
       }
      """)

    val testCases = Table[String, Long](
      ("expression", "expected"),
      ("M:throwAndCatch", 77),
    )

    forEvery(testCases) { (exp: String, num: Long) =>
      s"eval[$exp] --> $num" in {
        val expected: SResult = SResultFinalValue(SValue.SInt64(num))
        runUpdateExpr(pkgs)(e"$exp") shouldBe expected
      }
    }
  }

  "throw/catch (UserDefined)" should {

    // Basic throw/catch example a user defined exception

    val pkgs: PureCompiledPackages = typeAndCompile(p"""
       module M {

         record @serializable MyException = { message: Text } ;

         exception MyException = {
           message \(e: M:MyException) -> M:MyException {message} e
         };

         val throwAndCatch : Update Int64 =
           try @Int64 (upure @Int64 (throw @Int64 @M:MyException (M:MyException {message = "oops"})))
           catch e -> Some @(Update Int64) (upure @Int64 77);

       }
      """)

    val testCases = Table[String, Long](
      ("expression", "expected"),
      ("M:throwAndCatch", 77),
    )

    forEvery(testCases) { (exp: String, num: Long) =>
      s"eval[$exp] --> $num" in {
        val expected: SResult = SResultFinalValue(SValue.SInt64(num))
        runUpdateExpr(pkgs)(e"$exp") shouldBe expected
      }
    }
  }

  "throw/catch (UserDefined, with integer payload)" should {

    // Throw/catch example of a user defined exception, passing an integer payload.

    val pkgs: PureCompiledPackages = typeAndCompile(p"""
       module M {

         record @serializable MyException = { message: Text, payload : Int64 } ;

         exception MyException = {
           message \(e: M:MyException) -> M:MyException {message} e
         };

         val throwAndCatch : Update Int64 =
             try @Int64 (upure @Int64 (throw @Int64 @M:MyException (M:MyException {message = "oops", payload = 77})))
             catch e ->
               case (from_any_exception @M:MyException e) of
                  None -> Some @(Update Int64) (upure @Int64 999)
                | Some my -> Some @(Update Int64) (upure @Int64 (M:MyException {payload} my))
             ;
       }
      """)

    val testCases = Table[String, Long](
      ("expression", "expected"),
      ("M:throwAndCatch", 77),
    )

    forEvery(testCases) { (exp: String, num: Long) =>
      s"eval[$exp] --> $num" in {
        val expected: SResult = SResultFinalValue(SValue.SInt64(num))
        runUpdateExpr(pkgs)(e"$exp") shouldBe expected
      }
    }
  }

  "selective exception handling" should {

    // Example which nests two try-catch block around an expression which throws:
    // 3 variants: (with one source line changed,marked)
    // -- The inner handler catches (some)
    // -- The inner handler does not catch (none); allowing the outer handler to catch
    // -- The inner handler throws while deciding whether to catch

    val pkgs: PureCompiledPackages = typeAndCompile(p"""
      module M {
        record @serializable MyException = { message: Text } ;

        exception MyException = {
          message \(e: M:MyException) -> M:MyException {message} e
        };

        val innerCatch : Update Int64 =
            ubind x: Int64 <-
                try @Int64
                    ubind x: Int64 <-
                        try @Int64
                            upure @Int64 (throw @Int64 @M:MyException (M:MyException {message = "oops"}))
                        catch e ->
                            Some @(Update Int64) (upure @Int64 77)
                    in
                        upure @Int64 (ADD_INT64 200 x)
                catch e ->
                    Some @(Update Int64) (upure @Int64 88)
            in
                upure @Int64 (ADD_INT64 100 x)
         ;

        val outerCatch : Update Int64 =
            ubind x: Int64 <-
                try @Int64
                    ubind x: Int64 <-
                        try @Int64
                            upure @Int64 (throw @Int64 @M:MyException (M:MyException {message = "oops"}))
                        catch e ->
                            None @(Update Int64) // THIS IS THE ONLY LINE CHANGED
                    in
                        upure @Int64 (ADD_INT64 200 x)
                catch e ->
                    Some @(Update Int64) (upure @Int64 88)
            in
                upure @Int64 (ADD_INT64 100 x)
         ;

        val throwWhileInnerHandlerDecides : Update Int64 =
            ubind x: Int64 <-
                try @Int64
                    ubind x: Int64 <-
                        try @Int64
                            upure @Int64 (throw @Int64 @M:MyException (M:MyException {message = "oops"}))
                        catch e ->
                            throw @(Option (Update Int64)) @M:MyException (M:MyException {message = "oops"}) //CHANGE
                    in
                        upure @Int64 (ADD_INT64 200 x)
                catch e ->
                    Some @(Update Int64) (upure @Int64 88)
            in
                upure @Int64 (ADD_INT64 100 x)
         ;

       }
      """)

    val testCases = Table[String, Long](
      ("expression", "expected"),
      ("M:innerCatch", 377),
      ("M:outerCatch", 188),
      ("M:throwWhileInnerHandlerDecides", 188),
    )

    forEvery(testCases) { (exp: String, num: Long) =>
      s"eval[$exp] --> $num" in {
        val expected: SResult = SResultFinalValue(SValue.SInt64(num))
        runUpdateExpr(pkgs)(e"$exp") shouldBe expected
      }
    }
  }

  "selective exception handling (using payload)" should {

    // Selective handling, where either an inner or outer handler may access the exception payload:
    // 3 variantions, this time seleced dynamically using a pair of bool
    //
    // 1. True/False  -- The inner handler catches (some)
    // 2. False/False -- The inner handler does not catch (none); allowing the outer handler to catch
    // 3. False/True  -- The inner handler throws while deciding whether to catch
    //
    // The test result is an integer, composed by adding components demonstrating the control flow taken
    // 1000 -- exception payload or original throw
    // 2000 -- exception payload of secondary throw (variation-3)
    // 77 -- inner handler catches
    // 88 -- outer handler catches
    // 200 -- normal controlflow following inner catch
    // 100 -- normal controlflow following outer catch

    val pkgs: PureCompiledPackages = typeAndCompile(p"""
      module M {
        record @serializable MyException = { payload: Int64 } ;

        exception MyException = {
          message \(e: M:MyException) -> "no-message"
        };

        val maybeInnerCatch : Bool -> Bool -> Update Int64 = \(catchAtInner: Bool) (throwAtInner: Bool) ->
          ubind x: Int64 <-
            try @Int64
              ubind x: Int64 <-
                try @Int64
                  upure @Int64 (throw @Int64 @M:MyException (M:MyException {payload = 1000}))
                catch e ->
                  case catchAtInner of
                    True ->
                      (case (from_any_exception @M:MyException e) of
                        None -> None @(Update Int64)
                      | Some my -> Some @(Update Int64) (upure @Int64 (ADD_INT64 77 (M:MyException {payload} my))))
                  | False ->
                      (case throwAtInner of
                         True -> throw @(Option (Update Int64)) @M:MyException (M:MyException {payload = 2000})
                       | False -> None @(Update Int64))
              in
                upure @Int64 (ADD_INT64 200 x)
            catch e ->
              case (from_any_exception @M:MyException e) of
                None -> None @(Update Int64)
              | Some my -> Some @(Update Int64) (upure @Int64 (ADD_INT64 88 (M:MyException {payload} my)))

          in
            upure @Int64 (ADD_INT64 100 x) ;
       }
      """)

    val testCases = Table[String, Long](
      ("expression", "expected"),
      ("M:maybeInnerCatch True False", 1377),
      ("M:maybeInnerCatch False False", 1188),
      ("M:maybeInnerCatch False True", 2188),
    )

    forEvery(testCases) { (exp: String, num: Long) =>
      s"eval[$exp] --> $num" in {
        val expected: SResult = SResultFinalValue(SValue.SInt64(num))
        runUpdateExpr(pkgs)(e"$exp") shouldBe expected
      }
    }
  }

  "throw/catch (control flow)" should {

    // Another example allowing dynamic control of different test paths. Novelty here:
    // - uses GeneralError, with Text payload encoding an int
    // - variations 1..4 selected by an integer arg
    // - final result contains elements which demonstrate the control flow taken

    val pkgs: PureCompiledPackages = typeAndCompile(p"""
      module M {

        val myThrow : forall (a: *). (Text -> a) =
          /\ (a: *). \(mes : Text) ->
            throw @a @GeneralError (MAKE_GENERAL_ERROR mes);

        val isPayLoad : AnyException -> Text -> Bool =
          \(e: AnyException) (mes: Text) ->
            EQUAL @AnyException e (to_any_exception @GeneralError (MAKE_GENERAL_ERROR mes)) ;

        val extractPayload : AnyException -> Int64 =
          \(e: AnyException) ->
            case (M:isPayLoad e "payload2") of True -> 2 | False ->
            case (M:isPayLoad e "payload3") of True -> 3 | False ->
            1000000 ;

        val myCatch : forall (a: *). (Int64 -> a) -> (Text -> Update a) -> Update a =
          /\ (a: *). \ (handler: Int64 -> a) (body: Text -> Update a) ->
            try @a
              (body "body-unitish")
            catch e ->
              Some @(Update a) (upure @a (handler (M:extractPayload e))) ;

        val throwCatchTest : (Int64 -> Update Int64) = \ (x: Int64) ->
          ubind x: Int64 <-
            M:myCatch @Int64 (\(pay : Int64) -> ADD_INT64 100 pay) (\(u : Text) ->
              ubind x: Int64 <-
                M:myCatch @Int64 (\(pay : Int64) -> ADD_INT64 200 pay) (\(u : Text) ->
                  upure @Int64
                    (ADD_INT64 4000
                      (case (EQUAL @Int64 x 1) of True -> x | False ->
                       case (EQUAL @Int64 x 2) of True -> M:myThrow @Int64 "payload2" | False ->
                       case (EQUAL @Int64 x 3) of True -> M:myThrow @Int64 "payload3" | False ->
                       case (EQUAL @Int64 x 4) of True -> x | False ->
                       2000000)))
              in
                upure @Int64 (ADD_INT64 2000 x))
          in
            upure @Int64 (ADD_INT64 1000 x) ;

      }""")

    val testCases = Table[String, Long](
      ("expression", "expected"),
      ("M:throwCatchTest 1", 7001),
      ("M:throwCatchTest 2", 3202),
      ("M:throwCatchTest 3", 3203),
      ("M:throwCatchTest 4", 7004),
    )

    forEvery(testCases) { (exp: String, num: Long) =>
      s"eval[$exp] --> $num" in {
        val expected: SResult = SResultFinalValue(SValue.SInt64(num))
        runUpdateExpr(pkgs)(e"$exp") shouldBe expected
      }
    }
  }

  "throws from various places" should {

    // Example define a common "wrap" function which handles just a specific user exception:
    // Series of tests which may throw within its context:
    //
    // - example1 -- no thow
    // - example2 -- throw handled exception
    // - example3 -- throw unhandled exception
    // - example4 -- throw handled exception on left & right of a binary op
    // - example5 -- throw handled exception which computing the payload of an outer throw

    val pkgs: PureCompiledPackages = typeAndCompile(p"""
      module M {

        record @serializable MyExceptionH = { message: Text } ;

        // H,U -- handled/unhandled user exceptions

        exception MyExceptionH = {
          message \(e: M:MyExceptionH) -> M:MyExceptionH {message} e
        } ;

        record @serializable MyExceptionU = { message: Text } ;

        exception MyExceptionU = {
          message \(e: M:MyExceptionU) -> M:MyExceptionU {message} e
        } ;

        val wrap : (Unit -> Text) -> Update Text = \(body: Unit -> Text) ->
          try @Text (upure @Text (APPEND_TEXT "RESULT: " (body ())))
          catch e ->
            Some @(Update Text)
              upure @Text
                case (from_any_exception @M:MyExceptionH e) of
                  None -> "UNHANDLED"
                | Some my -> APPEND_TEXT "HANDLED: " (M:MyExceptionH {message} my) ;

        val example1 : Update Text =
          M:wrap (\(u: Unit) ->
            "Happy Path") ;

        val example2 : Update Text =
          M:wrap (\(u: Unit) ->
            throw @Text @M:MyExceptionH (M:MyExceptionH {message = "oops1"})) ;

        val example3 : Update Text =
          M:wrap (\(u: Unit) ->
            throw @Text @M:MyExceptionU (M:MyExceptionU {message = "oops2"})) ;

        val example4 : Update Text =
          M:wrap (\(u: Unit) ->
            APPEND_TEXT
              (throw @Text @M:MyExceptionH (M:MyExceptionH {message = "left"}))
              (throw @Text @M:MyExceptionH (M:MyExceptionH {message = "right"}))
          );

        val example5 : Update Text =
          M:wrap (\(u: Unit) ->
            throw @Text @M:MyExceptionH (throw @M:MyExceptionH @M:MyExceptionH (M:MyExceptionH {message = "throw-in-throw"}))
          );

      } """)

    val testCases = Table[String, String](
      ("expression", "expected"),
      ("M:example1", "RESULT: Happy Path"),
      ("M:example2", "HANDLED: oops1"),
      ("M:example3", "UNHANDLED"),
      ("M:example4", "HANDLED: left"),
      ("M:example5", "HANDLED: throw-in-throw"),
    )

    forEvery(testCases) { (exp: String, str: String) =>
      s"eval[$exp] --> $str" in {
        val expected: SResult = SResultFinalValue(SValue.SText(str))
        runUpdateExpr(pkgs)(e"$exp") shouldBe expected
      }
    }
  }

  private def typeAndCompile(pkg: Package): PureCompiledPackages = {
    val rawPkgs = Map(defaultParserParameters.defaultPackageId -> pkg)
    Validation.checkPackage(rawPkgs, defaultParserParameters.defaultPackageId, pkg)
    data.assertRight(
      PureCompiledPackages(rawPkgs, Compiler.Config.Default.copy(stacktracing = FullStackTrace))
    )
  }

  private def runExpr(pkgs1: PureCompiledPackages)(e: Expr): SResult = {
    Speedy.Machine.fromPureExpr(pkgs1, e).run()
  }

  private def runUpdateExpr(pkgs1: PureCompiledPackages)(e: Expr): SResult = {
    def transactionSeed: crypto.Hash = crypto.Hash.hashPrivateKey("ExceptionTest.scala")
    Speedy.Machine.fromScenarioExpr(pkgs1, transactionSeed, e).run()
  }

}
