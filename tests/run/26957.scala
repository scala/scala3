class Outer {
  val outerField: Unit = ()

  def takesLambda(value: () => Unit): Unit = {}

  new Super(y = takesLambda(() => outerField)) {}
}

class Super(x: Unit = (), y: Unit)

object Test:
  def main(args: Array[String]): Unit =
    Outer()
