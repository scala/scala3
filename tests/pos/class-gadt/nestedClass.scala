// Nested class for GADT constraining
object nestedClass{
  enum Expr[A] {
    case IntExpr(value: Int) extends Expr[Int]
    case Other[T](value: T) extends Expr[T]
  }

  class Outer1[C] {
    class Inner1 {
      def eval(e: Expr[C]): C =
        e match {
          case Expr.IntExpr(i) => i + 2
          case Expr.Other(v) => v
        }
    }
  }

  def foo2[C](): Unit = 
    class Outer2 {
      class Inner2 {
        def eval(e: Expr[C]): C =
          e match {
            case Expr.IntExpr(i) => i + 2
            case Expr.Other(v) => v
          }
      }
    }

  class Outer3[C] {
    def foo3(): Unit = 
      class Inner3 {
          def eval(e: Expr[C]): C =
            e match {
              case Expr.IntExpr(i) => i + 2
              case Expr.Other(v) => v
            }
      }
  }

  trait Outer4[C] {
    class Inner4 {
      def eval(e: Expr[C]): C =
        e match {
          case Expr.IntExpr(i) => i + 2
          case Expr.Other(v) => v
        }
    }
  }

  class Outer5[C] {
    object Inner5 {
      def eval(e: Expr[C]): C =
        e match {
          case Expr.IntExpr(i) => i + 2
          case Expr.Other(v) => v
        }
    }
  }
}