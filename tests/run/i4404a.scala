import java.lang.reflect.Modifier

class Car
final class Volvo extends Car

object Car {
  val car = new Car {}
}

object Test {
  def main(args: Array[String]): Unit = {
    val l =  List(new Car, new Volvo, Car, Car.car)
      .map(_.getClass)
      .map(cls => s"${Modifier.toString(cls.getModifiers)} $cls")
    println(l.mkString("\n"))
  }
}
