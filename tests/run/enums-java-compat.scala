trait JEnum[E <: JEnum[E]] { self: reflect.Enum =>
  final def name: String = productPrefix
}

trait JEnumCompanion[E <: JEnum[E]] {
  def valueOf(name: String): E
  def values: Array[E]
}

enum A extends JEnum[A] {
  case MONDAY, TUESDAY, SATURDAY
  case Stuff
  // case Someday(x: String) // uncommenting this line will prevent `object A` from compiling
  def report = "Reported"
}
object A extends JEnumCompanion[A]

trait Foo1
trait Bar

enum B(val gravity: Double)(val isItGood: Boolean) extends Foo1 {
  case EARTH extends B(9.8)(true)
  case JUPITER extends B(100)(true)
  case MOON extends B(4.3)(true)
  case Foo extends B(10)(true) with Bar
}

object Test {
  def main(args: Array[String]): Unit = {
    val t1 = B.EARTH
    val t2 = B.JUPITER

    println("ordinal:  " + t1.ordinal)
    println("toString: " + t1.toString)

    val values: Array[A] = A.values
    println("Values class: " + values.getClass)
    values.foreach(v => println(v.toString + " : " + v.ordinal))
    println("By-name value: " + A.valueOf("MONDAY"))
    try A.valueOf("stuff")
    catch { case e: IllegalArgumentException =>
      println("Correctly failed to retrieve illegal name, message: " + e.getMessage)
    }
  }
}
