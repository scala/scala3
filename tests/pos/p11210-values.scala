trait Foo[+A] {
  def toSeq: Seq[A] = ???
}
trait Bar[+A] {
  def toSeq: Seq[Seq[A]] = ???
}

@main def Test = {
  val foobar: Foo[Seq[Double]] & Bar[Double] = ???
  val m: Seq[Seq[Double]] = foobar.toSeq
}
