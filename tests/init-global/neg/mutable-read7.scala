object Positioned:
  var debug: Boolean = false
  var debugId = Int.MinValue
  var nextId: Int = 0

abstract class Positioned:
  if (Positioned.debug) { 
    println("do debugging")
  }

object Trees:
  class Tree extends Positioned
  val emptyTree = new Tree
// nopos-error: No warnings can be incurred under -Werror.