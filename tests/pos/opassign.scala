object opassign {

  var count: Int = 0

  def next = { count += 1; count }

  var x: Int = 0
  x += 1

  { var x: Int = 0
    x += 1
  }

  class Ref {
    var x: Int = compiletime.uninitialized
  }
  val r = new Ref
  r.x += 1

  val arr = new Array[Int](10)
  arr(0) += 1

  def f(x: Int): Ref = new Ref
  f(next).x += 1

  val buf = new collection.mutable.ListBuffer[Int]
  buf += 1
}
