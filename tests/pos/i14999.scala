trait Foo:
   def foo: String ?=> Int =
     summon[String].length

trait Bar extends Foo:
   override def foo =
      super.foo

class Baz extends Bar

@main def Test =
  given String = "hello"
  assert(Baz().foo == 5)
