package effekt
package context

import effekt.typer.TyperOps


abstract class Context extends TyperOps {

  // bring the context itself in scope
  implicit val context: Context = this

}
