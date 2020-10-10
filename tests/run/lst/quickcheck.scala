package lst

object QuickTest:
  import Iterator._
  val NumLimit = 30
  val iters = 200

  val integers = new Iterator[Int]:
    val rand = new java.util.Random
    def hasNext = true
    def next = rand.nextInt()

  val booleans: Iterator[Boolean] =
    integers.map(x => x > 0)

  def upto(end: Int): Iterator[Int] =
    integers.map(x => (x % end).abs)

  def lists: Iterator[Lst[Int]] =
    for n <- upto(5)
        list <- if n == 0 then single(Lst.Empty) else nonEmptyLists
    yield list

  def nonEmptyLists: Iterator[Lst[Int]] =
    for hd <- upto(NumLimit); tl <- lists
    yield hd :: tl

  def mapTest(xs: Lst[Int]) =
    assert(xs.map(_ * 2).toList == xs.toList.map(_ * 2))

  def filterTest(xs: Lst[Int]) =
    assert(xs.filter(_ % 2 == 0).toList == xs.toList.filter(_ % 2 == 0))

  def existsTest(xs: Lst[Int]) =
    assert(xs.exists(_ % 4 == 0) == xs.toList.exists(_ % 4 == 0))

  def forallTest(xs: Lst[Int]) =
    assert(xs.forall(_ % 4 != 0) == xs.toList.forall(_ % 4 != 0))

  def containsTest(xs: Lst[Int]) =
    assert(xs.contains(7) == xs.toList.contains(7))

  def reverseTest(xs: Lst[Int]) =
    assert(xs.reverse.toList == xs.toList.reverse)

  def test1(msg: String, test: Lst[Int] => Unit): Unit =
    print(s"$msg...")
    for xs <- lists.take(iters) do test(xs)
    println(" ok")

  def test() =
    test1("mapTest", mapTest)
    test1("filterTest", filterTest)
    test1("existsTest", existsTest)
    test1("forallTest", forallTest)
    test1("containsTest", containsTest)
    test1("reverseTest", reverseTest)

