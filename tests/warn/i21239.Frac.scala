package kse.maths

import scala.language.`3.6-migration`

opaque type Frac = Long
object Frac {
  inline def wrap(f: Long): kse.maths.Frac = f
  extension (f: Frac)
    inline def unwrap: Long = f
    inline def numerator: Int = ((f: Long) >>> 32).toInt
  extension (f: kse.maths.Frac)
    def +(g: Frac): kse.maths.Frac = f // eliding domain-specific addition logic
    def -(g: Frac): kse.maths.Frac =
      f + Frac.wrap(((-g.numerator).toLong << 32) | (g.unwrap & 0xFFFFFFFFL)) // warn
}
