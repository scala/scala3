object LazyZipTests {
  def apply(): Unit = {
    val a = List(1, 2)
    val b = List(10, 20)
    val c = List(100, 200)
    val d = List(1000, 2000)

    val r2 = a.lazyZip(b).map(_ + _)
    assert(r2 == List(11, 22))

    val r3 = a.lazyZip(b).lazyZip(c).map(_ + _ + _)
    assert(r3 == List(111, 222))

    val r4 = a.lazyZip(b).lazyZip(c).lazyZip(d).map(_ + _ + _ + _)
    assert(r4 == List(1111, 2222))
  }
}
