trait Bar
object Bar:
  inline given Bar = compiletime.error("Failed to expand!")
  val tests = summon[Bar] // error: Failed to expand!
