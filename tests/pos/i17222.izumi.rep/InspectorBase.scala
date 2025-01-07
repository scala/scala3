package izumi.reflect.dottyreflection

import scala.quoted.Quotes

trait InspectorBase extends ReflectionUtil {

  val qctx: Quotes
  import qctx.reflect._

  protected def shift: Int

  // FIXME reimplement TrivialMacroLogger on Scala 3
  inline def debug: debug = valueOf[debug]
  final type debug = false

  // println instead of report.info because report.info eats all the subsequent report.info's after first.
  inline final protected def logStart(inline s: String): Unit = {
    inline if (debug) println(" " * shift + currentPositionStr + s)
  }

  inline final protected def log(inline s: String): Unit = {
    inline if (debug) println(" " * shift + currentPositionStr + " -> " + s)
  }

  inline final protected def logTpeAttrs[T](inline typeRepr: TypeRepr): Unit = {
    inline if (debug) {
      val tree = TypeTree.of(using typeRepr.asType)
      val symbol = tree.symbol
      System
        .err.println(
          currentPositionStr + ": " +
          s"Attrs[${tree.show}]: type=${symbol.isType}, term=${symbol.isTerm}, packageDef=${symbol.isPackageDef}, classDef=${symbol.isClassDef}, typeDef=${symbol.isValDef}, defdef=${symbol.isDefDef}, bind=${symbol.isBind}, nosymbol=${symbol.isNoSymbol}"
        )
    }
  }

  private def currentPositionStr: String = {
    val pos = qctx.reflect.Position.ofMacroExpansion
    s"${pos.sourceFile.name}:${pos.endLine}"
  }

}

object InspectorBase {

  private[reflect] inline def ifDebug[A](inline f: => Unit): Unit = {
    inline if (valueOf[InspectorBase#debug]) {
//[error]              ^^^^^^^^^^^^^
//[error] izumi.reflect.dottyreflection.InspectorBase is not a legal path
//[error] since it has a member InternalTypeRefOrParamRef with possibly conflicting bounds Object{def underlying(ctx: Any): Nothing} <: ... <: Object{def underlying(ctx: Nothing): Matchable}
      f
    }
  }

  private[reflect] inline def log(inline shift: Int, s: String): Unit = {
    inline if (valueOf[InspectorBase#debug]) println(" " * shift + " -> " + s)
  }

}
