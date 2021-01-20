trait Bar
object Bar with
  inline given Bar = compiletime.error("Failed to expand!")
  val tests = summon[Bar] // error
