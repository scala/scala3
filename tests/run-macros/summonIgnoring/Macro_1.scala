//> using options -experimental
import scala.quoted._
class C1
trait TC[T] {
  def print(): Unit
}
object TC {
  implicit transparent inline def auto[T]: TC[T] = ${autoImpl[T]}
  def autoImpl[T: Type](using Quotes): Expr[TC[T]] =
    import quotes.reflect._
    if(TypeRepr.of[T].typeSymbol == Symbol.classSymbol("C1")){
      '{
        new TC[T] {
          def print() = {
            println("TC[C1] generated in macro")
          }
        }
      }
    } else {
      Expr.summonIgnoring[TC[C1]](Symbol.classSymbol("TC").companionModule.methodMember("auto")*) match
        case Some(a) =>
          '{
            new TC[T] {
              def print(): Unit =
                println(s"TC[${${Expr(TypeRepr.of[T].show)}}] generated in macro using:")
                $a.print()
            }
          }
        case None =>
          '{
            new TC[T]{
              def print(): Unit =
                println(s"TC[${${Expr(TypeRepr.of[T].show)}}] generated in macro without TC[C1]")
            }
          }
    }

}
