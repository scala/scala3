object Foo {
  type LeafElem[X] = X match {
    case String      => Char
    case Array[t]    => LeafElem[t]
    case Iterable[t] => LeafElem[t]
    case AnyVal      => X
  }

  def leafElem[X](x: X): LeafElem[X] =
    x match {
      case x: String      => x.charAt(0)
      case x: Array[t]    => leafElem(x(0))
      case x: Iterable[t] => leafElem(x.head)
      case _: AnyVal      => x
    }

  def leafElem2[X](x: X): LeafElem[X] =
    x match {
      case _: String   => leafElem[X](x)
      case w: Array[t] => leafElem[X](x)
    }
}

object Bar {
  import compiletime.ops.int.S

  type Plus[A <: Int, B <: Int] <: Int =
    A match {
      case 0    => B
      case S[p] => S[Plus[p, B]]
    }

  def plus[A <: Int, B <: Int](a: A, b: B): Plus[A, B] =
    a match {
      case _: 0    => b
      case a: S[p] => succ(plus(pred(a), b))
    }

  def pred[X <: Int](x: S[X]): X = (x - 1).asInstanceOf
  def succ[X <: Int](x: X): S[X] = (x + 1).asInstanceOf

  val nine: 9 = plus[4, 5](4, 5)
}

object Main {
  enum BinNat {
    case Zero
    // 2n + 1
    case Odd[N <: BinNat](n: N)
    // 2(n + 1)
    case Even[N <: BinNat](n: N)
  }

  import BinNat.*

  type Inc[N <: BinNat] <: BinNat =
    N match {
      case Zero.type => Odd[Zero.type]
      case Odd[n]    => Even[n]
      case Even[n]   => Odd[Inc[n]]
    }

  def inc[N <: BinNat](b: N): Inc[N] =
    b match {
      case _: Zero.type => new Odd(Zero)
      case o: Odd[n]    => new Even(o.n)
      case e: Even[n]   =>
        // 2(n + 1) + 1 = 2(inc(n)) + 1
        new Odd(inc(e.n))
    }
}
