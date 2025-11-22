trait Hierarchy {
  sealed trait Expr
}
trait If {
  this: Hierarchy =>
  case class If(cond: Expr, yes: Expr, no: Expr) extends Expr
}
trait Word {
  this: Hierarchy =>
  case class Word(name: String) extends Expr
}
trait IntExpr {
  this: Hierarchy =>
  case class IntExpr(value : Int) extends Expr
}

object SimpleExpr extends Hierarchy with If with Word with IntExpr
//object OtherExpr extends Hierarchy with If with IntExpr

object Demo {
  import SimpleExpr.*
  def func(expr: Expr) = expr match {
    case If(cond, yes, no) => cond
    case Word(name) => name
    // compiler should emit warning "missing case statement"
    // emits the wrong warning "unreachable code"
  }
}