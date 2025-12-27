// Test that Nullable annotations are working in Java files.

import javax.annotation.J

class S extends J {
  override def p(s: String): String = ??? // error
}
