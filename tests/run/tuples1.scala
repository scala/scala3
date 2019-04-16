object Test extends App {
  val x0 = (); println(x0)
  val x1 = 1 *: x0; println(x1)
  val x2 = ("A", 1); println(x2)
  val x3 = 2 *: x2; println(x3)
  val x4 = "B" *: x3; println(x4)
  val y0 = x4(0)
  val y1 = x4(1)
  val x5 = 3 *: x4; println(x5)
  val x6 = "C" *: x5; println(x6)
  val x7 = 4 *: x6; println(x7)
  val x8 = "D" *: x7; println(x8)
  val h1 = x1.head; val h1c: Int = h1; println(s"h1 = $h1")
  val h2 = x2.head; val h2c: String = h2; println(s"h2 = $h2")
  val h7 = x7.head; val h7c: Int = h7; println(s"h7 = $h7")
  val h8 = x8.head; val h8c: String = h8; println(s"h8 = $h8")
  val t1 = x1.tail; val t1c: Unit = t1; println(s"t1 = $t1")
  val t2 = x2.tail; val t2c: Int *: Unit = t2; println(s"t2 = $t2")
  val t7 = x7.tail; val t7c: (String, Int) = t7.tail.tail.tail.tail; println(s"t7 = $t7")
  val t8 = x8.tail; val t8c: Int = t8(6); println(s"t8 = $t8")
  val a1_0 = x1(0); val a1_0c: Int = a1_0; println(s"a1_0 = $a1_0")
  val a2_0 = x2(0); val a2_0c: String = a2_0; println(s"a2_0 = $a2_0")
  val a3_1 = x3(1); val a3_1c: String = a3_1; println(s"a3_1 = $a3_1")
  val a4_3 = x4(3); val a4_3c: Int = a4_3; println(s"a4_3 = $a4_3")
  val a6_4 = x6(4); val a6_4c: String = a6_4; println(s"a6_4 = $a6_4")
  val a8_0 = x8(0); val a8_0c: String = a8_0; println(s"a8_0 = $a8_0")
  val c0_0 = x0 ++ x0; val c0_0c: Unit = c0_0; println(s"c0_0 = $c0_0")
  val c0_1 = x0 ++ x1; val c0_1c: Int *: Unit = c0_1c; println(s"c0_1 = $c0_1")
  val c1_0 = x1 ++ x0; val c1_0c: Int *: Unit = c1_0c; println(s"c1_0 = $c1_0")
  val c0_4 = x0 ++ x4; val c0_4c: (String, Int, String, Int) = c0_4; println(s"c0_4 = $c0_4")
  val c4_0 = x4 ++ x0; val c4_0c: (String, Int, String, Int) = c4_0; println(s"c4_0 = $c4_0")
  val c1_1 = x1 ++ x1; val c1_1c: (Int, Int) = c1_1; println(s"c1_1 = $c1_1")
  val c1_8 = x1 ++ x8; val c1_8c: (Int, String, Int, String, Int, String, Int, String, Int) = c1_8; println(s"c1_8 = $c1_8")
  val c2_1 = x2 ++ x1; val c2_1c: (String, Int, Int) = c2_1; println(s"c2_1 = $c2_1")
  val c2_2 = x2 ++ x2; val c2_2c: (String, Int, String, Int) = c2_2; println(s"c2_2 = $c2_2")
  val c2_3 = x2 ++ x3; val c2_3c: (String, Int, Int, String, Int) = c2_3; println(s"c2_3 = $c2_3")
  val c3_3 = x3 ++ x3; val c3_3c: (Int, String, Int, Int, String, Int) = c3_3; println(s"c3_3 = $c3_3")

/* TODO: re-enable
  inline def decompose1 = inline x2 match { case x *: xs => (x, xs) }
  inline def decompose2 = inline x2 match { case x *: y *: xs => (x, y, xs) }

  { val (x, xs) = decompose1
    val xc: String = x
    val xsc: Int *: Unit = xs
    println(s"$x2 -> $x, $xs")
  }

  { val (x, y, xs) = decompose2
    val xc: String = x
    val yc: Int = y
    val xsc: Unit = xs
    println(s"$x2 -> $x, $y, $xs")
  }
*/
  val x3s: 3 = x3.size
  val us: 0 = ().size

// dynamic operations

  def head1(x: NonEmptyTuple): Tuple.Head[x.type] = x.head
  def head2[X <: NonEmptyTuple](x: X): Tuple.Head[X] = x.head

  val hd1: Int = head1(x3)
  val hd2: Int = head2(x3)

  def tail1(x: NonEmptyTuple): Tuple.Tail[x.type] = x.tail
  def tail2[X <: NonEmptyTuple](x: X): Tuple.Tail[X] = x.tail

  val tl1: (String, Int) = tail1(x3)
  val tl2: (String, Int) = tail2(x3)

  def elem[X <: NonEmptyTuple](x: X, n: Int): Tuple.Elem[X, n.type] = x(n)
  val elem1: String = x3(1)

  def toArray[X <: Tuple](x: X): Array[Object] = x.toArray
  val toArray1 = x3.toArray

  def cons[X, Y <: Tuple](x: X, y: Y): X *: Y = x *: y
  val cons1: Boolean *: Int *: (String, Int) = cons(true, x3)

  def concat[X <: Tuple, Y <: Tuple](x: X, y: Y): Tuple.Concat[X, Y] = x ++ y
  def concat0(x: Tuple, y: Tuple): Tuple.Concat[x.type, y.type] = x ++ y
  val conc1: (String, Int) = concat((), tl1)
  val conc2: (String, Int) = concat(tl1, ())
  val conc3: (String, Int, String, Int) = concat(tl1, tl1)
  val conc4: (String, Int) = concat0((), tl1)
  val conc5: (String, Int) = concat0(tl1, ())
  val conc6: (String, Int, String, Int) = concat0(tl1, tl1)

  def size[X <: Tuple](x: X): Tuple.Size[X] = x.size
  def size0(x: Tuple): Tuple.Size[x.type] = x.size
  val x3s0: 3 = size(x3)
  val us0: 0 = size(())
  val x3s1: 3 = size0(x3)
  val us1: 0 = size0(())
}
