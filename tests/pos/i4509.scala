import scala.language.experimental.erasedDefinitions

object Main {
  def fun[T](op: (erased x: Int) ?=> T) = op(using 0)
  fun { }
}
