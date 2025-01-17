import scala.language.experimental.namedTuples

object Test:
  // original code from issue https://github.com/scala/scala3/issues/20427
  type NT = NamedTuple.Concat[(hi: Int), (bla: String)]
  def foo(x: NT) =
    x.hi // error
    val y: (hi: Int, bla: String) = x
    y.hi // ok

  // SELECTOR (reduces to apply)
  def foo1(x: NT) =
    val res1 = x.hi // error
    summon[res1.type <:< Int]
    val y: (hi: Int, bla: String) = x
    val res2 = y.hi // ok
    summon[res2.type <:< Int]

  // toTuple
  def foo2(x: NT) =
    val res1 = x.toTuple
    summon[res1.type <:< (Int, String)]
    val y: (hi: Int, bla: String) = x
    val res2 = y.toTuple
    summon[res2.type <:< (Int, String)]

  // apply
  def foo3(x: NT) =
    val res1 = x.apply(1)
    summon[res1.type <:< String]
    val y: (hi: Int, bla: String) = x
    val res2 = y.apply(1)
    summon[res2.type <:< String]

  // size
  def foo4(x: NT) =
    class Box:
      final val res1 = x.size // final val constrains to a singleton type
      summon[res1.type <:< 2]
      val y: (hi: Int, bla: String) = x
      final val res2 = y.size // final val constrains to a singleton type
      summon[res2.type <:< 2]

  // head
  def foo5(x: NT) =
    val res1 = x.head
    summon[res1.type <:< Int]
    val y: (hi: Int, bla: String) = x
    val res2 = y.head
    summon[res2.type <:< Int]

  // last
  def foo6(x: NT) =
    val res1 = x.last
    summon[res1.type <:< String]
    val y: (hi: Int, bla: String) = x
    val res2 = y.last
    summon[res2.type <:< String]

  // init
  def foo7(x: NT) =
    val res1 = x.init
    summon[res1.type <:< (hi: Int)]
    val y: (hi: Int, bla: String) = x
    val res2 = y.init
    summon[res2.type <:< (hi: Int)]

  // tail
  def foo8(x: NT) =
    val res1 = x.tail
    summon[res1.type <:< (bla: String)]
    val y: (hi: Int, bla: String) = x
    val res2 = y.tail
    summon[res2.type <:< (bla: String)]

  // take
  def foo9(x: NT) =
    val res1 = x.take(1)
    summon[res1.type <:< (hi: Int)]
    val y: (hi: Int, bla: String) = x
    val res2 = y.take(1)
    summon[res2.type <:< (hi: Int)]

  // drop
  def foo10(x: NT) =
    val res1 = x.drop(1)
    summon[res1.type <:< (bla: String)]
    val y: (hi: Int, bla: String) = x
    val res2 = y.drop(1)
    summon[res2.type <:< (bla: String)]

  // splitAt
  def foo11(x: NT) =
    val res1 = x.splitAt(1)
    summon[res1.type <:< ((hi: Int), (bla: String))]
    val y: (hi: Int, bla: String) = x
    val res2 = y.splitAt(1)
    summon[res2.type <:< ((hi: Int), (bla: String))]

  // ++
  def foo12(x: NT) =
    val res1 = x ++ (baz = 23)
    summon[res1.type <:< (hi: Int, bla: String, baz: Int)]
    val y: (hi: Int, bla: String) = x
    val res2 = y ++ (baz = 23)
    summon[res2.type <:< (hi: Int, bla: String, baz: Int)]

  // map
  def foo13(x: NT) =
    val res1 = x.map([T] => (t: T) => Option(t))
    summon[res1.type <:< (hi: Option[Int], bla: Option[String])]
    val y: (hi: Int, bla: String) = x
    val res2 = y.map([T] => (t: T) => Option(t))
    summon[res2.type <:< (hi: Option[Int], bla: Option[String])]

  // reverse
  def foo14(x: NT) =
    val res1 = x.reverse
    summon[res1.type <:< (bla: String, hi: Int)]
    val y: (hi: Int, bla: String) = x
    val res2 = y.reverse
    summon[res2.type <:< (bla: String, hi: Int)]

  // zip
  def foo15(x: NT) =
    val res1 = x.zip((hi = "xyz", bla = true))
    summon[res1.type <:< (hi: (Int, String), bla: (String, Boolean))]
    val y: (hi: Int, bla: String) = x
    val res2 = y.zip((hi = "xyz", bla = true))
    summon[res2.type <:< (hi: (Int, String), bla: (String, Boolean))]

  // toList
  def foo16(x: NT) =
    val res1 = x.toList
    summon[res1.type <:< List[Tuple.Union[(Int, String)]]]
    val y: (hi: Int, bla: String) = x
    val res2 = y.toList
    summon[res2.type <:< List[Tuple.Union[(Int, String)]]]

  // toArray
  def foo17(x: NT) =
    val res1 = x.toArray
    summon[res1.type <:< Array[Object]]
    val y: (hi: Int, bla: String) = x
    val res2 = y.toArray
    summon[res2.type <:< Array[Object]]

  // toIArray
  def foo18(x: NT) =
    val res1 = x.toIArray
    summon[res1.type <:< IArray[Object]]
    val y: (hi: Int, bla: String) = x
    val res2 = y.toIArray
    summon[res2.type <:< IArray[Object]]
