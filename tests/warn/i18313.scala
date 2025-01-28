//> using options -Werror -Wunused:imports

import scala.deriving.Mirror

case class Test(i: Int, d: Double)
case class Decoder(d: Product => Test)

// OK, no warning returned
//val ok = Decoder(summon[Mirror.Of[Test]].fromProduct)
//
// returns warning:
// [warn] unused import
// [warn] import scala.deriving.Mirror
val d = Decoder(d = summon[Mirror.Of[Test]].fromProduct) // no warn
