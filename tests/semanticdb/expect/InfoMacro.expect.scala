import scala.quoted.*

object InfoMacro/*<-_empty_::InfoMacro.*/ {
  inline def reportInfo/*<-_empty_::InfoMacro.reportInfo().*/(msg/*<-_empty_::InfoMacro.reportInfo().(msg)*/: String/*->scala::Predef.String#*/): Unit/*->scala::Unit#*/ = ${ reportInfoMacro/*->_empty_::InfoMacro.reportInfoMacro().*/('msg) }

  def reportInfoMacro/*<-_empty_::InfoMacro.reportInfoMacro().*/(msg/*<-_empty_::InfoMacro.reportInfoMacro().(msg)*/: Expr/*->scala::quoted::Expr#*/[String/*->scala::Predef.String#*/])(using Quotes/*->scala::quoted::Quotes#*/): Expr/*->scala::quoted::Expr#*/[Unit/*->scala::Unit#*/] = {
    import quotes/*->scala::quoted::Quotes$package.quotes().*/.reflect/*->scala::quoted::Quotes#reflect.*/.report/*->scala::quoted::Quotes#reflectModule#report.*/

    // Report an info diagnostic
    report/*->scala::quoted::Quotes#reflectModule#report.*/.info/*->scala::quoted::Quotes#reflectModule#reportModule#info().*/(s/*->scala::StringContext#s().*/"Info from macro: ${msg/*->_empty_::InfoMacro.reportInfoMacro().(msg)*/.valueOrAbort/*->scala::quoted::Quotes#valueOrAbort().*/}")

    '{ () }
  }
}
