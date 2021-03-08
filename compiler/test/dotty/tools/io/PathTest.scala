package dotty.tools.io

import org.junit.Assert._
import org.junit.Test

class PathTest {
  // Ref https://github.com/lampepfl/dotty/issues/11644#issuecomment-792457275
  @Test def parent(): Unit = {
    assertEquals(Directory(".."), Path("").parent)
    assertEquals(Directory(".."), Path(".").parent)
    assertEquals(Directory("..") / "..", Path("..").parent)
    assertEquals(Directory("."), Path("foo.txt").parent)
    assertEquals(Directory("."), (Path(".") / "foo.txt").parent)
    assertEquals(Directory("."), (Path("bar") / ".").parent)
    assertEquals(Directory("."), (Path("foo") / ".." / "." / "bar").parent)
    assertEquals(Directory("baz") / "bar", (Path(".") / "baz" / "bar" / "foo.txt").parent)

    for (root <- Path.roots) {
      val rootDir = Directory(root.path)
      assertEquals(rootDir, root.parent)
      assertEquals(rootDir, (root / "foo.txt").parent)
      assertEquals(rootDir / "baz" / "bar", (root / "baz" / "bar" / "foo.txt").parent)
    }
  }
}
