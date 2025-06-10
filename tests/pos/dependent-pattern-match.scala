trait A:
  val x: Int
  type T
case class B(x: Int, y: Int) extends A:
  type T = String
  val z: T = "B"
case class C(x: Int) extends A:
  type T = Int
  val z: T = 1

object BParts:
  def unapply(b: B): Option[(b.x.type, b.y.type)] = Some((b.x, b.y))

def test1(a: A) = a match
  case b @ B(x, y) =>
    // b: a.type & B
    // x: (a.type & B).x.type
    // y: (a.type & B).y.type
    val e1: a.type = b
    val e2: a.x.type = b.x
    val e3: a.x.type = x
    val e4: b.x.type = x
    x + y
  case C(x) =>
    val e1: a.x.type = x
    x
  case BParts(x, y) =>
    val e1: a.x.type = x
    x + y
  case _ => 0

def test2(a: A): a.T =
  a match
    case b: B =>
      // b: a.type & B
      // b.z: b.T = (a & B)#T = a.T & String
      b.z
    case c: C => c.z

def test3(a: A): a.T =
  a match
    case b: B =>
      // b: a.type & B; hence b.T <:< a.T & String
      // We don't have a: b.type in the body,
      // so we can't prove String <:< a.T
      val x: b.T = b.z + ""
      x
    case c: C =>
      val x: c.T = c.z + 0
      x

def test4(x: A, y: A) =
  x match
    case z: y.type =>
      // if x.eq(y) then z = x = y,
      // z: x.type & y.type
      val a: x.type = z
      val b: y.type = z
    case _ =>