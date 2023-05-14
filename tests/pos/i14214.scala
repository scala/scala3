class Dummy
given Dummy = ???
trait Foo
given foo: Foo = ???
trait Bar
given bar(using Dummy): Bar = ???

object Test:
  summon[Dummy ?=> Foo] // was error
  summon[Dummy ?=> Foo](using foo)  // works
  summon[Dummy ?=> Foo](using (_: Dummy) ?=> foo)  // works
  summon[Dummy ?=> Bar]
  summon[Dummy ?=> Bar](using bar)  // works
  summon[Dummy ?=> Bar](using (_: Dummy) ?=> bar)  // works


