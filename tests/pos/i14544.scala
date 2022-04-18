import scala.language.strictEquality

case class MyClass[A](value: String)(val a: A) derives CanEqual

class Something {}
given CanEqual[Something, Something] = CanEqual.derived

val a = MyClass[Something]("some")(new Something())
val b = MyClass[Something]("some")(new Something())
val c = new Something()
val d = new Something()

def test1 = println(a == b)
def test2 = println(c == d)
