import A.*
object Test extends App {
  class Foo(f: => Foo)
  transparent inline implicit def foo(implicit f: => Foo): Foo = new Foo(summon[Foo])
  def summonFoo(implicit ev: Foo): Foo = ev
  summonFoo
}
