package scala.quoted.compiletime

trait Printer[T] {
  def show(x: T): String
}
