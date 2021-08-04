import a.*

object B {
  val foo = new A.Buf[Seq[Double]]
  val bar = Seq.empty[Double]
  foo.append(bar)
}
