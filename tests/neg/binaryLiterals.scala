
object Test:
  val x = 0b1__0000_0000_0000_0000__0000_0000_0000_0000 // error: number too large
  val X = 0B1__0000_0000_0000_0000__0000_0000_0000_0000 // error: number too large
  val y = 0b1__0000_0000_0000_0000__0000_0000_0000_0000__0000_0000_0000_0000__0000_0000_0000_0000L // error: number too large
  val Y = 0B1__0000_0000_0000_0000__0000_0000_0000_0000__0000_0000_0000_0000__0000_0000_0000_0000L // error: number too large
  0b // error: invalid literal number
  0b2 // error: invalid literal number
