import java.math.{BigDecimal => JBigDecimal}
import java.math.{BigInteger => JBigInteger}

opaque type CurrencyValue = BigDecimal

extension (value: CurrencyValue)
  def negate: CurrencyValue = value.bigDecimal.negate().nn

def testBigDecimalConversion(jbd: JBigDecimal): BigDecimal = jbd
def testBigIntConversion(jbi: JBigInteger): BigInt = jbi