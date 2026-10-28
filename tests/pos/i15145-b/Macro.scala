object Macro:
  import scala.quoted.*
  inline def myMacro(): Unit = ${impl()}
  private def impl()(using Quotes): Expr[Unit] = '{ () }

trait Feature:
  class DependentCaseClass
  object DependentCaseClass:
    def apply(): DependentCaseClass = ???

object ModuleWithFeature extends Feature

object Package:
  type DependentCaseClass2 = ModuleWithFeature.DependentCaseClass
  def DependentCaseClass2: ModuleWithFeature.DependentCaseClass.type = ???

object UsesModule:
  import ModuleWithFeature.DependentCaseClass
  val t = DependentCaseClass()
