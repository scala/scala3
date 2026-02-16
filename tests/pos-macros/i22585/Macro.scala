import scala.quoted.*

trait Hammer[I, O] {
  def hammer(input: I): O
}

object Hammer {
  inline def makeProductHammerMacro[I, O](): Hammer[I, O] =
    ${ makeProductHammerMacroImpl[I, O] }

  def makeProductHammerMacroImpl[I: Type, O: Type](using Quotes): Expr[Hammer[I, O]] =
    '{ makeHammer[I, O]() }

  inline def makeHammer[S, O](): Hammer[S, O] =
    new Hammer[S, O] {
      lazy val (hammer: Hammer[?, Int], idx: Int) = ???

      override def hammer(input: S): O = {
        hammer.hammer(???.asInstanceOf).asInstanceOf[O]
      }
    }
}