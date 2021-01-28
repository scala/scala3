object Test extends App{
  def foo(a: => Any*) = ()
  def bar(a: => Any*) = foo(a *)
  def baz(a: => Seq[Any]) = foo(a *)
  bar(???, ???)
  baz(Seq(???, ???))

  def foo1(a: => Any, b: => Any*) = ()
  foo1(???)
  foo1(???, ???, ???)

  def assertFails(a: => Any) = {
   var failed = false
   try {
     a
   } catch {
     case e => failed = true
   }
   assert(failed)
  }

  def forceLength(b: => Any*) = b.length
  assertFails(forceLength(???))

  def forceHead(b: => Any*) = b(0)
  assertFails(forceHead(1, ???))
}
