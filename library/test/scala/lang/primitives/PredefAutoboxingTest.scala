package scala.lang.primitives

import org.junit.Assert.*
import org.junit.Test

class PredefAutoboxingTest {
  @Test def unboxNullByte() =
    assertEquals(Predef.Byte2byte(null.asInstanceOf[java.lang.Byte]), 0.toByte)

  @Test def unboxNullShort() =
    assertEquals(Predef.Short2short(null.asInstanceOf[java.lang.Short]), 0.toShort)

  @Test def unboxNullCharacter() =
    assertEquals(Predef.Character2char(null.asInstanceOf[java.lang.Character]), 0.toChar)

  @Test def unboxNullInteger() =
    assertEquals(Predef.Integer2int(null.asInstanceOf[java.lang.Integer]), 0)

  @Test def unboxNullLong() =
    assertEquals(Predef.Long2long(null.asInstanceOf[java.lang.Long]), 0L)

  @Test def unboxNullFloat() =
    assertEquals(Predef.Float2float(null.asInstanceOf[java.lang.Float]), 0F, 0F)

  @Test def unboxNullDouble() =
    assertEquals(Predef.Double2double(null.asInstanceOf[java.lang.Double]), 0D, 0D)

  @Test def unboxNullBoolean() =
    assertEquals(Predef.Boolean2boolean(null.asInstanceOf[java.lang.Boolean]), false)
}
