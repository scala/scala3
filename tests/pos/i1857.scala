package commandeer


trait CommandeerDSL[Host] {
  trait Operation[T]
  type Op[T] <: Operation[T]
}

object CommandeerDSL {
  def apply[Host, DSL <: CommandeerDSL[Host]](host: Host)(implicit dsl: DSL): DSL = dsl
}

trait Foo {
  def bar(a: String, b: Int): Double
}

object Foo {
  implicit val fooDSL: FooDSL = new FooDSL {}
}

trait FooDSL extends CommandeerDSL[Foo] {
  sealed trait FooOperation[T] extends Operation[T]
  type Op[T] = FooOperation[T]

  case class Bar(a: String, b: Int) extends FooOperation[Double]
}

object RunMe {
  //import Foo.*
  def main(args: Array[String]): Unit = {
    println("Hi Mum")

    val kevin = CommandeerDSL(null.asInstanceOf[Foo])
    println(s"Found DSL for Foo: $kevin")
    val bar = kevin.Bar("bob", 3)
    println(s"Made a bar: $bar")
  }
}
