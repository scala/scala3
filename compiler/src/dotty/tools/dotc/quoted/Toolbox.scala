package dotty.tools.dotc.quoted

import dotty.tools.dotc.ast.tpd

import scala.quoted.Expr
import scala.quoted.Exprs.{LiftedExpr, TastyTreeExpr}
import scala.runtime.quoted._

/** Default runners for quoted expressions */
object Toolbox {
  import tpd._

  type Run
  type Show

  implicit def toolbox[T](implicit
      runSettings: Settings[Run] = Settings.run(),
      showSettings: Settings[Show] = Settings.show()
    ): Toolbox[T] = new Toolbox[T] {

    def run(expr: Expr[T]): T = expr match {
      case expr: LiftedExpr[T] =>
        expr.value
      case expr: TastyTreeExpr[Tree] @unchecked =>
        throw new Exception("Cannot call `Expr.run` on an `Expr` that comes from an inline macro argument.")
      case _ =>
        new QuoteDriver().run(expr, runSettings)
    }

    def show(expr: Expr[T]): String = new QuoteDriver().show(expr, showSettings)

  }

  class Settings[T] private (val outDir: Option[String], val rawTree: Boolean, val compilerArgs: List[String])

  object Settings {

    /** Quote run settings
     *  @param optimise Enable optimisation when compiling the quoted code
     *  @param outDir Output directory for the compiled quote. If set to None the output will be in memory
     *  @param compilerArgs Compiler arguments. Use only if you know what you are doing.
     */
    def run(
        optimise: Boolean = false,
        outDir: Option[String] = None,
        compilerArgs: List[String] = Nil
        ): Settings[Run] = {
      var compilerArgs1 = compilerArgs
      if (optimise) compilerArgs1 = "-optimise" :: compilerArgs1
      new Settings(outDir, false, compilerArgs1)
    }

    /** Quote show settings
     *  @param color Print output with colors
     *  @param rawTree Do not remove quote tree artifacts
     *  @param compilerArgs Compiler arguments. Use only if you know what you are doing.
     */
    def show(
        color: Boolean = false,
        rawTree: Boolean = false,
        compilerArgs: List[String] = Nil
        ): Settings[Show] = {
      var compilerArgs1 = compilerArgs
      compilerArgs1 = s"-color:${if (color) "always" else "never"}" :: compilerArgs1
      new Settings(None, rawTree, compilerArgs1)
    }

  }

}
