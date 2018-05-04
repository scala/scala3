object Test extends App {
  import dotty.tools.dotc.util.Lst

  val xs0: Lst[String] = Lst.Empty
  val xs1 = Lst("a")
  val xs2 = Lst("a", "b")
  val xs3 = Lst("a", "b", "c")
  val xs4 = Lst("a", "b", "c", "d")
  val xs5 = Lst("a", "b", "c", "d", "e")
  val xs10 = xs5 ++ xs5

  val is0: Lst[Int] = Lst.Empty
  val is1 = Lst(1)
  val is2 = Lst(1, 2)
  val is3 = Lst(1, 2, 3)
  val is4 = Lst(1, 2, 3, 4)
  val is5 = Lst(1, 2, 3, 4, 5)
  val is10 = is5 ++ is5

  def lengthTest() = {
    assert(xs0.length == 0)
    assert(xs1.length == 1)
    assert(xs2.length == 2)
    assert(xs10.length == 10)

    assert(is0.length == 0)
    assert(is1.length == 1)
    assert(is2.length == 2)
    assert(is10.length == 10)
  }

  def concatTest() = {
    assert(xs1 ++ xs0 == xs1)
    assert(xs0 ++ xs1 == xs1)
    assert(xs3 ++ xs0 == xs3)
    assert(xs0 ++ xs4 == xs4)

    assert(is1 ++ is0 == is1)
    assert(is0 ++ is1 == is1)
    assert(is3 ++ is0 == is3)
    assert(is0 ++ is4 == is4)
  }

  def foreachTest() = {
    xs0.foreach(s => assert(s.length == 1))
    xs3.foreach(s => assert(s.length == 1))
    def test1() = {
      var x = 0
      xs10.foreach(elem => x += elem.length)
    }
    def test2() = {
      var y = 0
      xs10.foreachInlined(elem => y += elem.length)
    }
    test1()
    test2()

    is0.foreach(i => assert(i == 1))
    is3.foreach(i => assert(i <= 3))
  }

  def mapTest() = {
    val ys0 = xs0.map(_.reverse)
    val ys1 = xs1.map(s => s + s)
    assert(ys1.mkString == "aa")
    val ys5 = xs5.map(s => s + s)
    assert(ys5.mkString == "aa, bb, cc, dd, ee")

    val js0 = is0.map(i => i * i)
    val js1 = is1.map(i => i + i)
    assert(js1.mkString == "2")
    val js5 = is5.map(s => s + s)
    assert(js5.mkString == "2, 4, 6, 8, 10")
  }

  def mapConserveTest() = {
    val ys0 = xs0.mapConserve(_.reverse)
    val ys1 = xs1.mapConserve(s => s + s)
    assert(ys1.mkString == "aa")
    val ys2 = xs2.mapConserve(identity)
    assert(ys2 `eqLst` xs2)
    val ys5 = xs5.map(s => s + s)
    assert(ys5.mkString == "aa, bb, cc, dd, ee")
    val ys4 = xs4.mapConserve(s => if (s == "c") "cc" else s)
    assert(ys4.mkString == "a, b, cc, d")

    val js0 = is0.mapConserve(i => i * i)
    val js1 = is1.mapConserve(s => s + s)
    assert(js1.mkString == "2")
    val js2 = is2.mapConserve(identity)
    assert(js2 `eqLst` is2)
    val js5 = is5.map(s => s + s)
    assert(js5.mkString == "2, 4, 6, 8, 10")
    val js4 = is4.mapConserve(s => if (s == 3) -3 else s)
    assert(js4.mkString == "1, 2, -3, 4")
  }

  def flatMapTest() = {
    val ys0 = xs0.flatMap(s => Lst(s, s))
    assert(ys0.isEmpty)
    val ys2 = xs2.flatMap(s => Lst(s, s))
    assert(ys2.mkString == "a, a, b, b")
    val ys2a = xs2.flatMap(_ => Lst.Empty)
    assert(ys2a.isEmpty)
    val ys4 = xs4.flatMap(s => Lst.fill(s.head - 'a')(s))
    assert(ys4.mkString == "b, c, c, d, d, d")
    val ys5 = xs5.flatMap(s => if s == "c" then Lst(s) else Lst())
    assert(ys5 == Lst("c"))

    val js0 = is0.flatMap(s => Lst(s, s))
    assert(js0.isEmpty)
    val js2 = is2.flatMap(s => Lst(s, s))
    assert(js2.mkString == "1, 1, 2, 2")
    val js2a = is2.flatMap(_ => Lst.Empty)
    assert(js2a.isEmpty)
    val js4 = is4.flatMap(s => Lst.fill(s - 1)(s))
    assert(js4.mkString == "2, 3, 3, 4, 4, 4", js4)
    val js5 = is5.flatMap(s => if s == 3 then Lst(-3) else Lst())
    assert(js5 == Lst(-3))
  }

  def filterTest() = {
    val ys0 = xs0.filter(_.head >= 'c')
    assert(ys0.isEmpty)
    val ys1 = xs1.filter(_.head >= 'c')
    assert(ys1.isEmpty)
    val ys1a = xs1.filterNot(_.head >= 'c')
    assert(ys1a `eqLst` xs1)
    val ys5 = xs5.filter(_.head % 2 != 0)
    assert(ys5 === Lst("a", "c", "e"), ys5)

    val js0 = is0.filter(_ > 3)
    assert(js0.isEmpty)
    val js1 = is1.filter(_ > 3)
    assert(js1.isEmpty)
    val js1a = is1.filterNot(_ > 3)
    assert(js1a `eqLst` is1)
    val js5 = is5.filter(_ % 2 != 0)
    assert(js5 === Lst(1, 3, 5), js5)
  }

  def existsTest() = {
    assert(!xs0.exists(_ => true))
    assert(xs1.exists(_ == "a"))
    assert(xs5.exists(_ == "c"))
    assert(!xs5.exists(_.head > 'e'))

    assert(!is0.exists(_ => true))
    assert(is1.exists(_ == 1))
    assert(is5.exists(_ == 3))
    assert(!is5.exists(_ > 5))
  }

  def forallTest() = {
    assert(xs0.forall(_ => false))
    assert(xs1.forall(_ == "a"))
    assert(xs5.forall(_.head <= 'e'))
    assert(!xs5.forall(_ == "c"))
  }

  def containsTest() = {
    assert(!xs0.contains(""))
    assert(xs1.contains("a"))
    assert(xs10.contains("e"))
    assert(!xs10.contains("f"))

    assert(!is0.contains(2))
    assert(is1.contains(1))
    assert(is10.contains(5))
    assert(!is10.contains(6))
  }

  def foldTest() = {
    assert(xs0.foldLeft("x")(_ ++ _) == "x")
    assert(xs1.foldLeft("x")(_ ++ _) == "xa")
    assert(xs2.foldLeft("x")(_ ++ _) == "xab")
    assert(xs3.foldLeft("x")(_ ++ _) == "xabc")
    assert(("x" /: xs0)(_ ++ _) == "x")
    assert(("x" /: xs1)(_ ++ _) == "xa")
    assert(("x" /: xs2)(_ ++ _) == "xab")
    assert(("x" /: xs3)(_ ++ _) == "xabc")
    assert(xs1.reduceLeft(_ + _) == "a")
    assert(xs3.reduceLeft(_ + _) == "abc")

    assert(is0.foldLeft(3)(_ + _) == 3)
    assert(is1.foldLeft(3)(_ + _) == 4)
    assert(is2.foldLeft(3)(_ + _) == 6)
    assert(is3.foldLeft(3)(_ + _) == 9)
    assert((3 /: is0)(_ + _) == 3)
    assert((3 /: is1)(_ + _) == 4)
    assert((3 /: is2)(_ + _) == 6)
    assert((3 /: is3)(_ + _) == 9)
    assert(is1.reduceLeft(_ + _) == 1)
    assert(is3.reduceLeft(_ + _) == 6)
  }

  def reverseTest() = {
    assert(xs0.reverse === xs0)
    assert(xs1.reverse === xs1)
    assert(xs3.reverse.mkString == "c, b, a", xs3.reverse.mkString)
    assert(xs4.reverse.reverse === xs4, xs4.reverse.reverse)
  }

  def applyTest() = {
    assert(xs5.head == "a")
    assert(xs5.last == "e")
    assert(xs5(3) == "d")
    assert(xs1(0) == "a")

    assert(is5.head == 1)
    assert(is5.last == 5)
    assert(is5(3) == 4)
    assert(is1(0) == 1)
  }

  def sliceTest() = {
    assert(xs5.slice(2, 4) === Lst("c", "d"))
    assert(xs5.drop(4) === Lst("e"))
    assert(xs5.take(4).mkString("") == "abcd")
    assert(xs5.drop(-1) `eqLst` xs5)
    assert(xs1.take(1) `eqLst` xs1)
    assert(xs0.take(10).length == 0)

    assert(is5.slice(2, 4) === Lst(3, 4))
    assert(is5.drop(4) === Lst(5))
    assert(is5.take(4).mkString("") == "1234")
    assert(is5.drop(-1) `eqLst` is5)
    assert(is1.take(1) `eqLst` is1)
    assert(is0.take(10).length == 0)
  }

  def zipWithTest() = {
    val ys4a = xs4.zipWith(xs5)(_ + _)
    val ys4b = xs5.zipWith(xs4)(_ + _)
    assert(ys4a.mkString("") == "aabbccdd", ys4a)
    assert(ys4a === ys4b)
    val ys1a = xs1.zipWith(xs1)(_ + _)
    assert(ys1a === Lst("aa"))
    val ys1b = xs1.zipWith(xs2)(_ + _)
    assert(ys1b === Lst("aa"))
    val ys1c = xs2.zipWith(xs1)(_ + _)
    assert(ys1c === Lst("aa"))
    val ys0a = xs1.zipWith(xs0)(_ + _)
    val ys0b = xs0.zipWith(xs1)(_ + _)
    assert((ys0a ++ ys0b).isEmpty)
    val ys3i = xs3.zipWithIndex.map((x, y) => (x, y + 1))
    assert(ys3i === Lst(("a", 1), ("b", 2), ("c", 3)), ys3i)

    val js4a = is4.zipWith(is5)(_ + _)
    val js4b = is5.zipWith(is4)(_ + _)
    assert(js4a.mkString("") == "2468", js4a)
    assert(js4a === js4b)
    val js1a = is1.zipWith(is1)(_ + _)
    assert(js1a === Lst(2))
    val js1b = is1.zipWith(is2)(_ + _)
    assert(js1b === Lst(2))
    val js1c = is2.zipWith(is1)(_ + _)
    assert(js1c === Lst(2))
    val js0a = is1.zipWith(is0)(_ + _)
    val js0b = is0.zipWith(is1)(_ + _)
    assert((js0a ++ js0b).isEmpty)
    val js3i = is3.zipWithIndex.map((x, y) => (x, y + 1))
    assert(js3i === Lst((1, 1), (2, 2), (3, 3)), js3i)
    assert(js3i.forall(_ == _))
  }

  def correspondsTest() = {
    assert(xs4.corresponds(xs4)(_ == _))
    assert(!xs4.corresponds(xs5)(_ == _))
    assert(xs1.corresponds(xs1)(_ == _))
    assert(!xs1.corresponds(Lst("b"))(_ == _))
    assert(xs0.corresponds(xs0)(_ == _))
    assert(!xs0.corresponds(xs1)(_ == _))
    val zs1 = Lst(new Object, new Object)
    val zs2 = Lst(zs1(0), zs1(1))
    val zs3 = Lst(new Object, new Object)
    assert(zs1.eqElements(zs1))
    assert(zs1.eqElements(zs2))
    assert(!zs1.eqElements(zs3))

    assert(is4.corresponds(is4)(_ == _))
    assert(!is4.corresponds(is5)(_ == _))
    assert(is1.corresponds(is1)(_ == _))
    assert(!is1.corresponds(Lst(-1))(_ == _))
    assert(is0.corresponds(is0)(_ == _))
    assert(!is0.corresponds(is1)(_ == _))
  }

  def bufferTest() = {
    { val b = new Lst.Buffer[String]
      b += "a"
      assert(b.size == 1)
      assert(b.toLst === Lst("a"))
      b += "aa"
      b ++= Lst.fill(20)("a")
      assert(b.toLst.mkString("") == "a" * 23)
      assert(b.size == 22)
    }

    { val b = new Lst.Buffer[Int]
      b += 1
      assert(b.size == 1)
      assert(b.toLst === Lst(1))
      b += 11
      b ++= Lst.fill(20)(1)
      assert(b.toLst.mkString("") == "1" * 23)
      assert(b.size == 22)
    }
  }

  println("testing")
  lengthTest()
  concatTest()
  foreachTest()
  mapTest()
  mapConserveTest()
  flatMapTest()
  filterTest()
  existsTest()
  forallTest()
  containsTest()
  foldTest()
  reverseTest()
  applyTest()
  sliceTest()
  zipWithTest()
  correspondsTest()
  bufferTest()
}