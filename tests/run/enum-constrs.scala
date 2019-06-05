enum Color extends compat.JEnum[Color] {
  case Red, Green, Blue
}

enum E[+T] extends compat.JEnum[E[_]] {
  case S1, S2
  case C extends E[Int]
}

enum Vehicle(wheels: Int) extends compat.JEnum[Vehicle] {
  case Bike extends Vehicle(2)
  case Car extends Vehicle(4)
}

object Test extends App {
  println(Color.Red)
  println(E.S1)
  println(Vehicle.Car)
}