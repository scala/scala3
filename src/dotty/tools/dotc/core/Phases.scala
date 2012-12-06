package dotty.tools.dotc
package core

object Phases {

  abstract class Phase {
    def erasedTypes: Boolean
  }


}