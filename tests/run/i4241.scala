object Test extends App {
  val a: PartialFunction[Int, Int] = { case x => x }
  val b: PartialFunction[Int, Int] = x => x match { case 1 => 1; case 2 => 2 }
  val c: PartialFunction[Int, Int] = x => { x match { case 1 => 1 } }
  val d: PartialFunction[Int, Int] = x => { { x match { case 1 => 1 } } }

  val e: PartialFunction[Int, Int] = x => { println("foo"); x match { case 1 => 1 } }
  val f: PartialFunction[Int, Int] = x => x
  val g: PartialFunction[Int, String] = { x => x.toString }
  val h: PartialFunction[Int, String] = _.toString
  assert(a.isDefinedAt(2))
  assert(b.isDefinedAt(2))
  assert(!b.isDefinedAt(3))
  assert(c.isDefinedAt(1))
  assert(!c.isDefinedAt(2))
  assert(d.isDefinedAt(1))
  assert(!d.isDefinedAt(2))
  assert(e.isDefinedAt(2))
  assert(f.isDefinedAt(2))
  assert(g.isDefinedAt(2))
  assert(h.isDefinedAt(2))
}


