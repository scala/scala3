
class A {
  def f: PartialFunction[Int, Int] =
    a => { (try a catch { case e : Throwable => throw e}) match { case n => n } }
}
