package scala.quoted
package util

/** An abstraction for variable definition to use in a quoted program.
 *  It decouples the operations of get and update, if needed to be spliced separately.
 */
sealed trait Var[T] {

  // Retrieves the value of the variable
  def get given QuoteContext: Expr[T]

  // Update the variable with the expression of a value (`e` corresponds to the RHS of variable assignment `x = e`)
  def update(e: Expr[T]) given QuoteContext: Expr[Unit]
}

object Var {
  /** Create a variable initialized with `init` and used in `body`.
   *  `body` recieves a `Var[T]` argument which exposes `get` and `update`.
   *
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
   */
  def apply[T: Type, U: Type](init: Expr[T])(body: Var[T] => Expr[U]) given QuoteContext: Expr[U] = '{
    var x = $init
    ${
      body(
        new Var[T] {
          def get given QuoteContext: Expr[T] = 'x
          def update(e: Expr[T]) given QuoteContext: Expr[Unit] = '{ x = $e }
        }
      )
    }
  }
}
