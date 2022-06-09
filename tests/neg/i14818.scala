object M {
  type A
  type B
  def a = 1
  def b(x: Int) = x
}

object T1:
  export M.{A, B as A}  // error

object T2:
  export M.{A as B, *}

object T3:
  export M.{a as b, *}
  val x = b(1)  // error

object T4:
  export M.{A as C, B as C} // error

object T5:
  export M.{A as B, B as A}  // OK

