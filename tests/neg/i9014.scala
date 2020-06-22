trait Bar
object Bar:
  inline given as Bar = compiletime.error("Failed to expand!")
  val tests = summon[Bar] // error
