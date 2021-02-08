enum Filter:
  case IsOdd
  case IsPrime
  case LessThan(bound: Int)
  case And(f1: Filter, f2: Filter)

  def  apply(x: Int): Boolean = this match
    case IsOdd           => x % 2 == 0
    case IsPrime         => (2 until x).forall(y => x % y != 0)
    case LessThan(bound) => x < bound
    case And(f1, f2)     => f1(x) && f2(x)

def filter(f: Filter, elems: List[Int]): List[Int] = elems match
  case Nil => Nil
  case x :: xs =>
    if f(x) then x :: filter(f, xs)
    else filter(f, xs)

val xs = List(1, 2, 3, 4, 5)

import Filter.*

@main def Test =
  println(filter(IsOdd      , xs))
  println(filter(IsPrime    , xs))
  println(filter(LessThan(4), xs))