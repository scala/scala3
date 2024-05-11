import language.experimental.into

class BigInt(x: Int):
  def + (other: into BigInt): BigInt = ???
  def * (other: into BigInt): BigInt = ???

object BigInt:
  given Conversion[Int, BigInt] = BigInt(_)

  extension (x: into BigInt)
    def + (other: BigInt): BigInt = ???
    def * (other: BigInt): BigInt = ???

@main def Test =
  val x = BigInt(2)
  val y = 3
  val a1 = x + y // uses conversion on `y`
  val a2 = y * x // uses conversion on `y`
  val a3 = x * x
  val a4 = y + y

