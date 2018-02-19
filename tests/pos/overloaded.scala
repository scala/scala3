object overloaded {

  def f(x: String): String = x
  def f[T >: Null](x: T): Int = 1

  val x1 = f("abc")
  val x2 = f(new Integer(1))
  val x3 = f(null)

  val x4: String => String = f
  val x5: String => Any = f
  val x6: Any = f _

  def g(): Int = 1
  def g(x: Int): Int = 2

  val y1: Int => Int = g
  val y2: Any = g _

  println(g())

  val xs = List("a", "b")
  xs.mkString

  def map(f: Char => Char): String = ???
  def map[U](f: Char => U): Seq[U] = ???
  val r1 = map(x => x.toUpper)
  val t1: String = r1
  val r2 = map(x => x.toInt)
  val t2: Seq[Int] = r2

  val rp1 = map { case x => x.toUpper }
  val tp1: String = rp1
  val rp2 = map { case x => x.toInt }
  val tp2: Seq[Int] = rp2

  def flatMap(f: Char => String): String = ???
  def flatMap[U](f: Char => Seq[U]): Seq[U] = ???
  val r3 = flatMap(x => x.toString)
  val t3: String = r3
  val r4 = flatMap(x => List(x))
  val t4: Seq[Char] = r4

  val rp3 = flatMap { case x => x.toString }
  val tp3: String = rp3
  val rp4 = flatMap { case x => List(x) }
  val tp4: Seq[Char] = rp4

  def bar(f: (Char, Char) => Unit): Unit = ???
  def bar(f: Char => Unit) = ???
  bar((x, y) => ())
  bar (x => ())
  // bar { case (x, y) => () } // error: cannot test if value types are references
  bar { case x => () }

  def combine(f: (Char, Int) => Int): Int = ???
  def combine(f: (String, Int) => String): String = ???
  val r5 = combine((x: Char, y) => x + y)
  val t5: Int = r5
  val r6 = combine((x: String, y) => x ++ y.toString)
  val t6: String = r6

  // Errors: The argument types of an anonymous function must be fully known
  // val rp5 = combine { case (x: Char, y) => x + y }
  // val tp5: Int = rp5
  // val rp6 = combine { case (x: String, y) => x ++ y.toString }
  // val tp6: String = rp6
}
