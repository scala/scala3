object Test extends dotty.runtime.LegacyApp {
  def id[A](a: => A): A = null.asInstanceOf[A]
  def foo(f: (=> Int) => Int) = () => f(???)
  foo(id)() // should be allowed and not throw ???
}
