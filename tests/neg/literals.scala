trait RejectedLiterals {

  def missingHex: Int    = { 0x }        // error: invalid literal number

  def leadingZeros: Int  = { 01 }        // line 6: no leading zero

  def tooManyZeros: Int  = { 00 }        // line 8: no leading zero

  def zeroOfNine: Int    = { 09 }        // line 10: no leading zero

  def orphanDot: Int     = { 9. }        // error: ident expected

  def zeroOfNineDot: Int = { 09. }       // error: malformed integer, ident expected

  def noHexFloat: Double = { 0x1.2 }     // error: ';' expected but double literal found.

}

trait Braceless {

  def missingHex: Int    = 0x            // error: was: not reported, taken as zero

  def leadingZeros: Int  = 01            // line 24: no leading zero

  def tooManyZeros: Int  = 00            // line 26: no leading zero

  def zeroOfNine: Int    = 09            // line 28: no leading zero

  def orphanDot: Int     = 9.            // should be: ident expected

  def zeroOfNineDot: Int = 09.           // error: an identifier expected, but 'def' found, shoule be ident expected

  def noHexFloat: Double = 0x1.2         // should be: ';' expected but double literal found.
}

trait MoreSadness {

  def tooTiny: Float     = { 0.7e-45f }      // error: floating point number too small

  def twoTiny: Double    = { 2.0e-324 }      // error: double precision floating point number too small

  def tooHuge: Float     = { 3.4028236E38f } // error: floating point number too large

  def twoHuge: Double    = { 1.7976931348623159e308 } // error: double precision floating point number too large
}

trait Lengthy {

  def bad = 1l

  def worse = 123l
}
