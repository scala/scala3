// The body of a polymorphic function literal is created once and shared by all
// type applications, so it cannot be converted to a SAM type, which might be
// implemented by an anonymous class.
trait MySam[A]:
  def apply(x: A): A

abstract class MyAbstractSam[A]:
  def apply(x: A): A

trait Init[A]:
  println("init")
  def apply(x: A): A

def test =
  val f1: [A] => MySam[A] = [A] => (x: A) => x // error
  val f2: [A] => Runnable = [A] => () => () // error
  val f3: [A] => java.util.function.Supplier[List[A]] = [A] => () => Nil // error
  val f4: [A] => MyAbstractSam[A] = [A] => (x: A) => x // error
  val f5: [A] => Init[A] = [A] => (x: A) => x // error
  val f6: [A] => PartialFunction[A, Int] = [A] => (x: A) => x match { case i: Int => i } // error
