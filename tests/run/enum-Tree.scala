enum Tree[T] {
  case True                 extends Tree[Boolean]
  case False                extends Tree[Boolean]
  case Zero                 extends Tree[Int]
  case Succ(n: Tree[Int])   extends Tree[Int]
  case Pred(n: Tree[Int])   extends Tree[Int]
  case IsZero(n: Tree[Int]) extends Tree[Boolean]
  case If(cond: Tree[Boolean], thenp: Tree[T], elsep: Tree[T])
                            extends Tree[T]
}

object Test {
  import Tree._

  def eval[T](e: Tree[T]): T = e match {
    case True => true
    case False => false
    case Zero => 0
    case Succ(f) => eval(f) + 1
    case Pred(f) => eval(f) - 1
    case IsZero(f) => eval(f) == 0
    case If(cond, thenp, elsep) => if (eval(cond)) eval(thenp) else eval(elsep)
  }

  val data = If(IsZero(Pred(Succ(Zero))), Succ(Succ(Zero)), Pred(Pred(Zero)))

  def main(args: Array[String]) = {
    println(s"$data --> ${eval(data)}")
  }
}
