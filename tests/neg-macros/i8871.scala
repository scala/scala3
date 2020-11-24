import scala.quoted._
object Macro {
  def impl[A : Type](using Quotes): Unit = {
    import quotes.reflect._
    val tpe = TypeRepr.of[A].asType.asInstanceOf[Type[_ <: AnyRef]]
    '{ (a: ${tpe}) => ???} // error
  }
}
