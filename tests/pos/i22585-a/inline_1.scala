import scala.quoted.*

trait Hammer[I, O] {
  def hammer(input: I): O
}

object Hammer {
  inline def makeHammer[S, O](): Hammer[S, O] =
    new Hammer[S, O] {
      lazy val (hammer: Hammer[?, Int], idx: Int) = ???

      override def hammer(input: S): O = {
        hammer.hammer(???.asInstanceOf).asInstanceOf[O]
      }
    }
}
