
object DCEUtils {
  @scala.annotation.internal.DoNotDCE
  def shouldDCE(expr: => Any): Unit = try {
    expr
    throw new Exception("Expected DCE")
  } catch {
    case dce: dotty.runtime.DeadCodeEliminated =>
        // TODO: check stack trace to see if the DCE was in the fist call of expr
  }
}
