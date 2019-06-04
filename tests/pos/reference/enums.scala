package enums

object t1 {

enum Color {
  case Red, Green, Blue
}

}

object t2 {

enum Color {
  case Red
  case Green
  case Blue
}
}

object t3 {

enum Color(val rgb: Int) {
  case Red   extends Color(0xFF0000)
  case Green extends Color(0x00FF00)
  case Blue  extends Color(0x0000FF)
}

}


enum Planet(mass: Double, radius: Double) {
  private final val G = 6.67300E-11
  def surfaceGravity = G * mass / (radius * radius)
  def surfaceWeight(otherMass: Double) =  otherMass * surfaceGravity

  case MERCURY extends Planet(3.303e+23, 2.4397e6)
  case VENUS   extends Planet(4.869e+24, 6.0518e6)
  case EARTH   extends Planet(5.976e+24, 6.37814e6)
  case MARS    extends Planet(6.421e+23, 3.3972e6)
  case JUPITER extends Planet(1.9e+27,   7.1492e7)
  case SATURN  extends Planet(5.688e+26, 6.0268e7)
  case URANUS  extends Planet(8.686e+25, 2.5559e7)
  case NEPTUNE extends Planet(1.024e+26, 2.4746e7)
}

object Planet {
  def main(args: Array[String]) = {
    val earthWeight = args(0).toDouble
    val mass = earthWeight/EARTH.surfaceGravity
    for (p <- values)
      println(s"Your weight on $p is ${p.surfaceWeight(mass)}")
  }
}

