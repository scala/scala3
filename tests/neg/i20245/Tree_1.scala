package effekt
package source

import effekt.context.Context

object Resolvable {

  // There need to be two resolve extension methods for the error to show up
  // They also need to take an implicit Context
  extension (n: Int) {
    def resolve(using C: Context): Unit = ???
  }

  extension (b: Boolean) {
    def resolve(using C: Context): Unit = ???
  }
}
export Resolvable.resolve
