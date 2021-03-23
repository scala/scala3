
object Foo {
  val foo = scala.collection.mutable.ArrayBuffer.empty[Seq[Double]]
  val bar = Seq.empty[Double]
  foo.append(bar)
  foo.append(Seq(bar):_*)
}
