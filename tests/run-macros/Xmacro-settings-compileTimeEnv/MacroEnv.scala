import scala.quoted.*

object MacroEnv {

 transparent inline def get(inline key:String):Option[String] = ${
    getImpl('key)
 }

 def getImpl(key:Expr[String])(using Quotes):Expr[Option[String]] = {
    import quotes.reflect.*
    val retval = getInMacro(key.valueOrAbort)
    Expr(retval)
 }

 def getInMacro(key:String)(using Quotes):Option[String] = {
    import quotes.reflect.*
    val keyEq = key + "="
    CompilationInfo.XmacroSettings.collectFirst{
      case v if v == key => ""
      case v if v.startsWith(keyEq) =>
                  v.substring(keyEq.length)
    }
 }

}
