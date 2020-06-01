sealed trait Base[A, +B]
case class Foo[A, +B](f: A => B) extends Base[A, B]

def bar(x: Base[Any, Any]): Unit = x match { case Foo(_) => }
