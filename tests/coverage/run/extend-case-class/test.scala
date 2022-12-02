// see issue 15835
import java.math.MathContext
case class DecimalConf(mathContext: MathContext, scaleLimit: Int, digitsLimit: Int)
object DecimalConf extends DecimalConf(MathContext.UNLIMITED, 6178, 308)

@main
def Test: Unit =
  val c = DecimalConf(MathContext.DECIMAL32, 1, 0)
  println(c.scaleLimit)
  println(DecimalConf.scaleLimit)
