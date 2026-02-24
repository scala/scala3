import language.experimental.captureChecking
trait Region[R^]
def id[T](x: T): T = ???
def foo[R^](r: Region[R]): Unit =
  val t2 = () => id[Object^{R}](???)  // was error, now ok
