import caps.internal.paramAlias

class JavaRand()

val jrand: JavaRand^ = JavaRand()

class Rand(val self: JavaRand^):
  def this() = this(jrand)
  def this(s: JavaRand^, dummy: Int) = this(s)
  def this(d1: Int, s: JavaRand^) = this(s, d1)

val rand = Rand()
val _: Rand{val self: JavaRand^}^ = rand
val _: Rand = rand // error
val rand1 = Rand(jrand, 0)
val _: Rand{val self: JavaRand^{jrand}}^{jrand} = rand1
val _: Rand = rand1 // error
val rand2 = Rand(0, jrand)
val _: Rand{val self: JavaRand^{jrand}}^{jrand} = rand2
val _: Rand = rand2 // error
val rand3 = Rand(JavaRand(): JavaRand^)
val rand4 = Rand(jrand)

