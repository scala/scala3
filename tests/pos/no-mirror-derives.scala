class Foo derives Bar

trait Bar[T]
object Bar {
  def derived[T]: Bar[T] = new Bar[T] {}
}
