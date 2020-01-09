import org.junit.Test
import org.junit.Assert.assertEquals

// Test discovery should not pick this up because it's a trait
trait TraitTest {
  @Test def foo = {
    ???
  }
}
