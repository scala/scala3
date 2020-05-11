package scala.quoted

package object staging:

  /** Evaluate the contents of this expression and return the result.
   *  It provides a new `scala.quoted.Scope` that is only valid within the scope the argument.
   *
   *  Usage:
   *  ```
   *  val e: T = run { // (using s: Scope) =>
   *    expr
   *  }
   *  ```
   *  where `expr: s.Expr[T]`
   */
  def run[T](expr: (s: Scope) ?=> s.Expr[T])(using toolbox: Toolbox): T = toolbox.run(expr(using _))

  /** Provide a new quote scope within the scope of the argument that is only valid within the scope the argument.
   *  Return the result of the argument.
   *
   *  Usage:
   *  ```
   *  val e: T = usingNewScope { // (using s: Scope) =>
   *    thunk
   *  }
   *  ```
   *  where `thunk: T`
   */
  def usingNewScope[T](thunk: Scope ?=> T)(using toolbox: Toolbox): T =
    val noResult = new Object
    var result: T = noResult.asInstanceOf[T]
    def dummyRun(using s: Scope): s.Expr[Unit] = {
      result = thunk
      '{}
    }
    toolbox.run(dummyRun(using _))
    assert(result != noResult) // toolbox.run should have thrown an exception
    result
  end usingNewScope

end staging
