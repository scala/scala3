package scala.tasty.interpreter.abstr

class Var(private var value: Any) extends Ref {
  def get = value

  def update(rhs: Any): Unit = {
    value = rhs
  }
}
