object C:
  def m(x: Float, y: Float): Unit = ()

object O:
  C.m(
    5.5, // OK
    0.12345678901234567 // warn - too precise for a float
  )
