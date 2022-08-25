package json.value.spec.parser

import java.math.MathContext
import com.github.plokhotnyuk.jsoniter_scala.core.JsonReader
sealed case class DecimalConf(mathContext: MathContext, scaleLimit: Int, digitsLimit: Int)


object DecimalConf extends DecimalConf(MathContext.UNLIMITED, 6178, 308)


