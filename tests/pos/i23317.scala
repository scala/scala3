import scala.quoted.*

def goImpl(using Quotes): Expr[Int] =
  List.empty[Type[?]] match
    case Nil =>
      Expr(0)
    case '[t1] :: Nil =>
      Expr(1)
    case '[t1] :: '[t2] :: Nil =>
      Expr(2)
    case '[t1] :: '[t2] :: '[t3] :: Nil =>
      Expr(3)
    case '[t1] :: '[t2] :: '[t3] :: '[t4] :: Nil =>
      Expr(4)
    case '[t1] :: '[t2] :: '[t3] :: '[t4] :: '[t5] :: Nil =>
      Expr(5)
    case '[t1] :: '[t2] :: '[t3] :: '[t4] :: '[t5] :: '[t6] :: Nil =>
      Expr(6)
    case '[t1] :: '[t2] :: '[t3] :: '[t4] :: '[t5] :: '[t6] :: '[t7] :: Nil =>
      Expr(7)
    case '[t1] :: '[t2] :: '[t3] :: '[t4] :: '[t5] :: '[t6] :: '[t7] :: '[t8] :: Nil =>
      Expr(8)
    case '[t1] :: '[t2] :: '[t3] :: '[t4] :: '[t5] :: '[t6] :: '[t7] :: '[t8] :: '[t9] :: Nil =>
      Expr(9)
    case _ =>
      Expr(999)
