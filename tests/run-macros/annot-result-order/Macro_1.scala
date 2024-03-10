//> using options -experimental -Yno-experimental

import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted._

@experimental
class print(msg: String) extends MacroAnnotation:
  def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    import quotes.reflect._
    def printMsg(msg: String) =
      val valSym = Symbol.newVal(Symbol.spliceOwner, Symbol.freshName("print"), TypeRepr.of[Unit], Flags.Private, Symbol.noSymbol)
      val valRhs =
        given Quotes = valSym.asQuotes
        '{ println(${Expr(msg)}) }.asTerm
      ValDef(valSym, Some(valRhs))
    List(printMsg(s"before: $msg"), tree, printMsg(s"after: $msg"))
