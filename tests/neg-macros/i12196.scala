import scala.quoted.*

def qqq(s: String)(using Quotes): Expr[Unit] = '{()}

inline val InlineVal = "i"
inline def InlineDef = "i"

inline def withInlineVal = ${ qqq(InlineVal) }
inline def withInlineDef = ${ qqq(InlineDef) } // error
inline def withString    = ${ qqq("i") }
