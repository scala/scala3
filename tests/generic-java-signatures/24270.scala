final class Box[A](value: A) extends AnyVal

trait Foo:
  def foo[A](a: A): Box[A] = Box(a)

object bar extends Foo

object Test:
  def main(args: Array[String]): Unit =
    println("bar:")
    Class.forName("bar").getMethods.sortBy(_.getName).filter(_.getName.contains("foo")).foreach(m => {
      println(m)
      println(m.toGenericString)
    })
    println("bar$:")
    Class.forName("bar$").getMethods.sortBy(_.getName).filter(_.getName.contains("foo")).foreach(m => {
      println(m)
      println(m.toGenericString)
    })
