object Test {

  def test1() =
    (new Function0[Unit] {
      def apply() = println("hello")
    })()

  val cond = true
  val foo = () => println("hi")
  val bar = () => println("there")

  val baz = (x: Int) => println(x)

  def test2() =
    (if (cond) foo else bar)()

  def test2a() =
    (if (cond) baz else baz)(33)

  def test3() =
    (try foo
     catch { case ex: Exception => bar }
     finally ())()

  def test4() =
    (cond match {
      case true => foo
      case false => bar
    })()

  def main(args: Array[String]) = {
    test1()
    test2()
    test2a()
    test3()
    test4()
  }
}
