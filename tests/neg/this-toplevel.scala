@main def main(): Unit =
  val x = this // error
  println(x)

def notMain(): Unit =
  val x = this // error
  println(x)

def inType(): Unit =
  val l: List[this.type] = ??? // error
  ???

object O:
  @main def inObj(): Unit =
    val x = this // OK
    println(x)

package explicitPackage:
  @main def main(): Unit =
    val x = this // error
    println(x)

  def notMain(): Unit =
    val x = this // error
    println(x)

  object O:
    @main def inObj(): Unit =
      val x = this // OK
      println(x)

