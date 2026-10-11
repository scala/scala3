package foo

class Bippy: // warn
  def foo = 1

class BIPPY:
  def foo = 2

object Dingo: // warn
  val bar = 0

object DINGO:
  val bar = 42

case class Hyrax() { // warn
  object X
}
object HyRaX {
  object Y
}
