import java.lang.reflect.Modifier

class Car
final class Volvo extends Car

object Car {
  val car = new Car {}
}

object Test {
  def main(args: Array[String]) = {
    List(new Car, new Volvo, Car, Car.car)
      .map(_.getClass.getModifiers)
      .foreach { m =>
        println(Modifier.isPrivate(m))
        println(Modifier.isFinal(m))
      }
  }
}
