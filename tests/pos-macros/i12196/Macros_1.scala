
import scala.quoted.*

inline val InlineStringVal = "abc"

inline def withInlineVal = ${ qqq(InlineStringVal) }

def qqq(s: String)(using Quotes): Expr[String] = Expr(s)
