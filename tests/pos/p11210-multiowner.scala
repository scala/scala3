trait Foo[A] {
  def append(elem: A): Unit = {}
}
trait Bar[A] {
  def append(elems: A*): Unit = {}
}

@main def Test = {
  val foobar: Foo[Seq[Double]] & Bar[Seq[Double]] = ???
  val seq = Seq.empty[Double]
  foobar.append(seq)
}
