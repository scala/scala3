import scala.tools.nsc.interpreter.IMain

object Test extends dotty.runtime.LegacyApp {
  val engine = new IMain.Factory getScriptEngine()
  engine.asInstanceOf[IMain].settings.usejavacp.value = true
  engine put ("n", 10)
  engine eval "1 to n.asInstanceOf[Int] foreach print"
}
