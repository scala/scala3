import scala.util.continuations._

class Use {
  val a = new Baz
  def bar: (Foo with BarA) @cpsParam[Unit, Unit] = a.foo
}
