object `gadt-inference` {
  enum Expr[T] {
    case StrLit(s: String) extends Expr[String]
    case IntLit(i: Int) extends Expr[Int]
  }
  import Expr._

  def eval[T](e: Expr[T]) =
    e match {
      case StrLit(s) =>
        val a = (??? : T) : String
        s : T
      case IntLit(i) =>
        val a = (??? : T) : Int
        i : T
    }

  def nested[T](o: Option[Expr[T]]) =
    o match {
      case Some(e) => e match {
        case StrLit(s) =>
          val a = (??? : T) : String
          s : T
        case IntLit(i) =>
          val a = (??? : T) : Int
          i : T
      }
      case None => ???
    }

  def local[T](e: Expr[T]) = {
    def eval[T](e: Expr[T]) =
      e match {
        case StrLit(s) =>
          val a = (??? : T) : String
          s : T
        case IntLit(i) =>
          val a = (??? : T) : Int
          i : T
      }

    eval(e) : T
  }
}
