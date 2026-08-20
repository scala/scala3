object OptionExample {
  def f(x: Any) = ()
  def example = {
    f(
      if (
        false
      )
    ) // error
    val someVal // error
