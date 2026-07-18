enum Color extends java.lang.Enum[Color] {
  case Red, Green, Blue
}

enum Vehicle(wheels: Int) extends java.lang.Enum[Vehicle] {
  case Bike extends Vehicle(2)
  case Car extends Vehicle(4)
}

object Test extends App {
  println(Color.Red)
  println(Vehicle.Car)
}