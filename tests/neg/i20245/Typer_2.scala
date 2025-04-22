//> using options -explain-cyclic
package effekt
package typer

import effekt.util.messages.ErrorReporter

import effekt.context.{ Context }

// This import is also NECESSARY for the cyclic error
import effekt.source.{ resolve } // error


trait TyperOps extends ErrorReporter { self: Context =>

  // passing `this` as ErrorReporter here is also NECESSARY for the cyclic error
  private[typer] val unification = new Unification(using this) // error

  // this export is NECESSARY for the cyclic error
  export unification.{ requireSubtype }

  // vvvvvvvv insert a line here, save, and run `compile` again vvvvvvvvvv
}





