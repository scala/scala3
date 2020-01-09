import org.junit.Test
import org.junit.Assert.assertEquals

// Test discovery should not pick this up because it's abstract
abstract class AbstractClassTest {
  @Test def foo = {
    ???
  }
}
