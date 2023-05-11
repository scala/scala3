trait Magic[S]:
  def init: S
  def step(s: S): String

object IntMagic extends Magic[Int]:
  def init = 0
  def step(s: Int): String = (s - 1).toString

object StrMagic extends Magic[String]:
  def init = "hi"
  def step(s: String): String = s.reverse

object Main:
  def onestep[T](m: () => Magic[T]): String = m().step(m().init)
  def unostep[T](m:    => Magic[T]): String = m.step(m.init)

  val iter: Iterator[Magic[?]] = Iterator.tabulate(Int.MaxValue)(i => if i % 2 == 0 then IntMagic else StrMagic)

  // was: class java.lang.String cannot be cast to class java.lang.Integer
  def main(args: Array[String]): Unit =
    onestep(() => iter.next()) // error
    unostep(iter.next())       // error
    val m = iter.next()
    unostep(m)                 // ok, because m is a value
