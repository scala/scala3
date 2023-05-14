
// f-interpolator wasn't doing any escape processing
class C:
  val octal = 8
  val fEscape = f"\u$octal%04x" // error
