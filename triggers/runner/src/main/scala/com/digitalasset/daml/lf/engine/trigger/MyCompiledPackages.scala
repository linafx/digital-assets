package com.daml.lf.engine.trigger

import com.daml.lf.CompiledPackages
import com.daml.lf.data.Ref.PackageId
import com.daml.lf.language.Ast._
import com.daml.lf.speedy.SExpr.SDefinitionRef
import com.daml.lf.speedy.{Compiler, SExpr}

private[lf] final class MyCompiledPackages(
    packages: Map[PackageId, Package],
    defns: Map[SDefinitionRef, SExpr],
    compilerConfig: Compiler.Config,
) extends CompiledPackages(compilerConfig) {
  override def packageIds: Set[PackageId] = packages.keySet
  override def getPackage(pkgId: PackageId): Option[Package] = packages.get(pkgId)
  override def getDefinition(dref: SDefinitionRef): Option[SExpr] = defns.get(dref)
}

private[lf] object MyCompiledPackages {

  /** Important: use this method only if you _know_ you have all the definitions! Otherwise
    * use the other apply, which will compile them for you.
    */
  def apply(
      packages: Map[PackageId, Package],
      defns: Map[SDefinitionRef, SExpr],
      compilerConfig: Compiler.Config,
  ): MyCompiledPackages =
    new MyCompiledPackages(packages, defns, compilerConfig)

  private def strip(p : Package): Package =
    Package(p.modules.mapValues(strip(_)), p.directDeps, p.languageVersion, p.metadata)

  private def strip(m : Module): Module =
    Module(m.name, m.definitions.mapValues(strip(_)), m.featureFlags)

  private def strip(d : Definition): Definition =
    d match {
      case DTypeSyn(_, _) => d
      case DDataType(serializable, params, cons) => cons match {
        case DataRecord(fields, optTpl) => DDataType(serializable, params, DataRecord(fields, optTpl.map(strip(_))))
        case DataVariant(variants) => DDataType(serializable, params, DataVariant(variants))
        case DataEnum(cons) => DDataType(serializable, params, DataEnum(cons))
      }
      case DValue(a, b, _, d) => DValue(a, b, null, d)
    }

  private def strip(t: Template): Template =
    t.copy(
      precond = null,
      signatories = null,
      agreementText = null,
      observers = null,
      choices = t.choices.mapValues(strip(_))
    )

  private def strip(t: TemplateChoice) =
    t.copy(
      controllers = null,
      update = null
    )

  def apply(
      packages: Map[PackageId, Package],
      compilerConfig: Compiler.Config = Compiler.Config.Default,
  ): Either[String, MyCompiledPackages] =
    Compiler
      .compilePackages(packages, compilerConfig)
      .map(apply(packages.mapValues(strip(_)), _, compilerConfig))

}
