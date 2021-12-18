final val G = 6.67300E-11

enum Planet(mass: Double, radius: Double) extends Enum[Planet]:
  def surfaceGravity = G * mass / (radius * radius)
  def surfaceWeight(otherMass: Double) =  otherMass * surfaceGravity

  case Mercury extends Planet(3.303e+23, 2.4397e6)
  case Venus   extends Planet(4.869e+24, 6.0518e6)

val check = summon[Planet.type <:< scala.reflect.EnumCompanion[Planet]]