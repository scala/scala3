import annotation.nowarn
object T {
  @deprecated def f = 1
  def t1 = / // error
  @nowarn    // unused-nowarn is not issued if earlier phase has an error.
  def t2 = f // no warning, refchecks doesn't run if typer has an error
}
