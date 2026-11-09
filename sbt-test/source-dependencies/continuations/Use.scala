import scala.util.continuations._

class Use {
  val a = new Baz
  def bar: (Foo & BarA) @cpsParam[Unit, Unit] = a.foo
}
