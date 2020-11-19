class Test {
  def res(x: quoted.Expr[Int])(using quoted.Reflection): quoted.Expr[Int] = x match { // error
    case '{ 1 + $b } => // error: Type must be fully defined. Consider annotating the splice using a type ascription: (${b}: XYZ).
      b // error: Not found: b
  }
}