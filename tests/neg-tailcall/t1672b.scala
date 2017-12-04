object Test1772B {
  @annotation.tailrec
  def bar : Nothing = { // error: TailRec optimisation not applicable
    try {
      throw new RuntimeException
    } catch {
      case _: Throwable => bar
    } finally {
      bar
    }
  }

  @annotation.tailrec
  def baz : Nothing = { // error: TailRec optimisation not applicable
    try {
      throw new RuntimeException
    } catch {
      case _: Throwable => baz
    } finally {
      ???
    }
  }

  @annotation.tailrec
  def boz : Nothing = { // error: TailRec optimisation not applicable
    try {
      throw new RuntimeException
    } catch {
      case _: Throwable => boz; ???
    }
  }

  @annotation.tailrec
  def bez : Nothing = { // error: TailRec optimisation not applicable
    try {
      bez
    } finally {
      ???
    }
  }

  // the `liftedTree` local method will prevent a tail call here.
  @annotation.tailrec
  def bar(i : Int) : Int = { // error: TailRec optimisation not applicable
    if (i == 0) 0
    else 1 + (try {
      throw new RuntimeException
    } catch {
      case _: Throwable => bar(i - 1) // old error
    })
  }
}
