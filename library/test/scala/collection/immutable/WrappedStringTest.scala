package scala.collection.immutable

import org.junit.Test
import org.junit.Assert.*

class WrappedStringTest {

  @Test // scala/bug#11518
  def indexOf_nonChar(): Unit = {
    assertEquals(-1, new WrappedString("test").indexOf[Any]("not a Char")) // doesn't overflow
  }
}
