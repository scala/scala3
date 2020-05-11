package scala.quoted
package util

/** An abstraction for variable definition to use in a quoted program.
 *  It decouples the operations of get and update, if needed to be spliced separately.
 */
sealed trait Var[T] {

  // Retrieves the value of the variable
  def get(using s: Scope): s.Expr[T]

  // Update the variable with the expression of a value (`e` corresponds to the RHS of variable assignment `x = e`)
  def update(using s: Scope)(e: s.Expr[T]): s.Expr[Unit]
}

object Var {
  /** Create a variable initialized with `init` and used in `body`.
   *  `body` receives a `Var[T]` argument which exposes `get` and `update`.
   *
   *  ```
   *  Var('{7}) {
   *    x => '{
   *      while(0 < ${x.get})
   *        ${x.update('{${x.get} - 1})}
   *      ${x.get}
   *    }
   *  }
   *
   *  will create the equivalent of:
   *
   *  '{
   *    var x = 7
   *    while (0 < x)
   *      x = x - 1
   *    x
   *  }
   *  ```
   */
  // FIXME
  def apply[T, U](using s: Scope)(init: s.Expr[T])(body: Var[T] => s.Expr[U])(using s.Type[T], s.Type[U]): s.Expr[U] = ??? /*'{
    var x = $init
    ${
      body(
        new Var[T] {
          def get(using s: Scope): s.Expr[T] = 'x
          def update(using s: Scope)(e: s.Expr[T]): s.Expr[Unit] = '{ x = $e }
        }
      )
    }
  }*/
}
