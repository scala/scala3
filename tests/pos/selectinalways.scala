class Buf[A] {
  def append(a: A): this.type = this
  def append(a: A*): this.type = this
}

@main def Test = {
  val foo = new Buf[Seq[Double]]
  val bar = Seq.empty[Double]
  foo.append(bar)
}
