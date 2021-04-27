import scala.quoted.*
object Macro {
  def impl[A : Type](using Quotes): Unit = {
    import quotes.reflect.*
    val tpe = TypeRepr.of[A].asType.asInstanceOf[Type[_ <: AnyRef]]
    '{ (a: ${tpe}) => ???} // error
  }
}
