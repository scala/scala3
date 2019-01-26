object Test extends App {

  // Explicit ADT
  enum IExp {
    case Lit(i: Int)
    case Neg(e: IExp)
    case Add(l: IExp, r: IExp)
  }

  val fe: IExp = {
    import IExp._
    Add(Lit(8), Neg(Add(Lit(1), Lit(2))))
  }

  // Base algebra
  trait Exp[T] {
    def lit(i: Int): T
    def neg(t: T): T
    def add(l: T, r: T): T
  }

  def tf0[T] with (e: Exp[T]): T =
    e.add(e.lit(8), e.neg(e.add(e.lit(1), e.lit(2))))

  object ExpSyntax {
    def lit[T](i: Int)     with (e: Exp[T]): T = e.lit(i)
    def neg[T](t: T)       with (e: Exp[T]): T = e.neg(t)
    def add[T](l: T, r: T) with (e: Exp[T]): T = e.add(l, r)
  }
  import ExpSyntax._ // It's safe to always have these in scope

  def tf1[T] with Exp[T]: T =
    add(lit(8), neg(add(lit(1), lit(2))))

  // Base operations as typeclasses
  instance of Exp[Int] {
    def lit(i: Int): Int = i
    def neg(t: Int): Int = -t
    def add(l: Int, r: Int): Int = l + r
  }

  instance of Exp[String] {
    def lit(i: Int): String = i.toString
    def neg(t: String): String = s"(-$t)"
    def add(l: String, r: String): String = s"($l + $r)"
  }

  println(tf1[Int])
  println(tf1[String])

  // Added case in algebra: *
  trait Mult[T] {
    def mul(l: T, r: T): T
  }
  object MultSyntax {
    def mul[T](l: T, r: T) with (e: Mult[T]): T = e.mul(l, r)
  }
  import MultSyntax._

  def tfm1[T: Exp : Mult] = add(lit(7), neg(mul(lit(1), lit(2))))
  def tfm2[T: Exp : Mult] = mul(lit(7), tf1)

  instance of Mult[Int] {
    def mul(l: Int, r: Int): Int = l * r
  }

  instance of Mult[String] {
    def mul(l: String, r: String): String = s"$l * $r"
  }

  println(tfm1[Int])
  println(tfm1[String])
  println(tfm2[Int])
  println(tfm2[String])

  // Added operation: Deserialization
  enum Tree {
    case Leaf(s: String)
    case Node(s: String, ts: Tree*)
  }
  import Tree._

  instance of Exp[Tree] with Mult[Tree] {
    def lit(i: Int): Tree = Node("Lit", Leaf(i.toString))
    def neg(t: Tree): Tree = Node("Neg", t)
    def add(l: Tree, r: Tree): Tree = Node("Add", l , r)
    def mul(l: Tree, r: Tree): Tree = Node("Mult", l , r)
  }

  val tf1Tree = tf1[Tree]
  val tfm1Tree = tfm1[Tree]

  println(s"tf1Tree = $tf1Tree")
  println(s"tfm1Tree = $tfm1Tree")

  class CanThrow private ()

  object CanThrow {
    private class Exc(msg: String) extends Exception(msg)
    def _throw(msg: String) with CanThrow: Nothing = throw new Exc(msg)
    def _try[T](op: CanThrow |=> T)(handler: String => T): T = {
      val ct = new CanThrow()
      try op with ct
      catch {
        case ex: Exception => handler(ex.getMessage)
      }
    }
  }
  import CanThrow._

  type Maybe[T] = CanThrow |=> T

  def show[T](op: Maybe[T]): Unit =
    println(_try(op.toString)(identity))

  def assertEquals[T](op1: Maybe[T], op2: Maybe[T]): Unit =
    _try {
      val x1 = op1
      val x2 = op2
      assert(x1 == x2, "$x1 != $x2")
    } {
      msg => assert(false, s"thrown: $msg")
    }

  def readInt(str: String): Maybe[Int] =
    _try(str.toInt)(_ => _throw(s"""Not a number: "$str""""))

  show(readInt("2"))
  show(readInt("X"))

  def fromTree[T](t: Tree) with Exp[T]: Maybe[T] = t match {
    case Node("Lit", Leaf(n)) => lit(readInt(n))
    case Node("Neg", t) => neg(fromTree(t))
    case Node("Add", l , r) => add(fromTree(l), fromTree(r))
    case _ => _throw(s"Invalid tree $t")
  }

  show(fromTree[Int](tf1Tree))
  show(fromTree[String](tf1Tree))
  show(fromTree[Tree](tf1Tree))

  trait Wrapped {
    def value[T] with Exp[T]: T
  }

  instance of Exp[Wrapped] {
    def lit(i: Int) = new Wrapped {
      def value[T] with (e: Exp[T]): T = e.lit(i)
    }
    def neg(t: Wrapped) = new Wrapped {
      def value[T] with (e: Exp[T]): T = e.neg(t.value)
    }
    def add(l: Wrapped, r: Wrapped) = new Wrapped {
      def value[T] with (e: Exp[T]): T = e.add(l.value, r.value)
    }
  }

  show {
    val t = fromTree[Wrapped](tf1Tree)
    s"${t.value[Int]}\n${t.value[String]}"

  }
  def fromTreeExt[T](recur: => Tree => Maybe[T]) with Exp[T]: Tree => Maybe[T] = {
    case Node("Lit", Leaf(n)) => lit(readInt(n))
    case Node("Neg", t) => neg(recur(t))
    case Node("Add", l , r) => add(recur(l), recur(r))
    case t => _throw(s"Invalid tree $t")
  }

  def fix[A](f: (=> A) => A): A = f(fix(f))

  def fromTree2[T: Exp](t: Tree): Maybe[T] = fix(fromTreeExt[T])(t)

  def fromTreeExt2[T](recur: => Tree => Maybe[T]) with Exp[T], Mult[T]: Tree => Maybe[T] = {
    case Node("Mult", l , r) => mul(recur(l), recur(r))
    case t => fromTreeExt(recur)(t)
  }

  def fromTree3[T: Exp : Mult](t: Tree): Maybe[T] = fix(fromTreeExt2[T])(t)

  assertEquals(fromTree[String](tf1[Tree]), tf1[String])
  assertEquals(fromTree2[String](tf1[Tree]), tf1[String])
  assertEquals(fromTree3[String](tf1[Tree]), tf1[String])
  assertEquals(fromTree3[String](tfm1[Tree]), tfm1[String])

  enum NCtx { case Pos, Neg }

  instance NegExp[T] with (e: Exp[T]) of Exp[NCtx => T] {
    import NCtx._
    def lit(i: Int) = {
      case Pos => e.lit(i)
      case Neg => e.neg(e.lit(i))
    }
    def neg(x: NCtx => T): NCtx => T = {
     case Pos => x(Neg)
     case Neg => x(Pos)
    }
    def add(l: NCtx => T, r: NCtx => T): NCtx => T =
      c => e.add(l(c), r(c))
  }

  println(tf1[NCtx => String](NCtx.Pos))

  def pushNeg[T](e: NCtx => T): T = e(NCtx.Pos)

  println(pushNeg(tf1[NCtx => String]))

  println(pushNeg(pushNeg(pushNeg(tf1))): String)

}