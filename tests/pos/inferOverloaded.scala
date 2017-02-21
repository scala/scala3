class MySeq[T] {
  def map1[U](f: T => U): MySeq[U] = new MySeq[U]
  def map2[U](f: T => U): MySeq[U] = new MySeq[U]
}

class MyMap[A, B] extends MySeq[(A, B)] {
  def map1[C](f: (A, B) => C): MySeq[C] = new MySeq[C]
  def map1[C, D](f: (A, B) => (C, D)): MyMap[C, D] = new MyMap[C, D]
  def map1[C, D](f: ((A, B)) => (C, D)): MyMap[C, D] = new MyMap[C, D]

  def foo(f: Function2[Int, Int, Int]): Unit = ()
  def foo[R](pf: PartialFunction[(A, B), R]): MySeq[R] = new MySeq[R]
}

object Test {
  val m = new MyMap[Int, String]

  // This one already worked because it is not overloaded:
  m.map2 { case (k, v) => k - 1 }

  // These already worked because preSelectOverloaded eliminated the non-applicable overload:
  m.map1(t => t._1)
  m.map1((kInFunction, vInFunction) => kInFunction - 1)
  val r1 = m.map1(t => (t._1, 42.0))
  val r1t: MyMap[Int, Double] = r1

  // These worked because the argument types are known for overload resolution:
  m.map1({ case (k, v) => k - 1 }: PartialFunction[(Int, String), Int])
  m.map2({ case (k, v) => k - 1 }: PartialFunction[(Int, String), Int])

  // These ones did not work before:
  m.map1 { case (k, v) => k }
  val r = m.map1 { case (k, v) => (k, k*10) }
  val rt: MyMap[Int, Int] = r
  m.foo { case (k, v) => k - 1 }

  // Used to be ambiguous but overload resolution now favors PartialFunction
  def h[R](pf: Function2[Int, String, R]): Unit = ()
  def h[R](pf: PartialFunction[(Double, Double), R]): Unit = ()
  h { case (a: Double, b: Double) => 42: Int }
}
