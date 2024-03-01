package dotty.tools.io

import org.junit.Test

class PathTest {
  // Ref https://github.com/scala/scala3/issues/11644#issuecomment-792457275
  @Test def parent(): Unit = {
    testParent(Path(""), Directory(".."))
    testParent(Path("."), Directory(".."))
    testParent(Path(".") / ".", Directory(".."))
    testParent(Path(".."), Directory("..") / "..")
    testParent(Path("..") / ".", Directory("..") / "..")
    testParent(Path("..") / "..", Directory("..") / ".." / "..")
    testParent(Path(".") / "..",
      Directory("..") / "..",
      Directory(".") / ".." / "..")

    testParent(Path("foo") / ".", Directory("."))
    testParent(Path("foo") / ".." / "bar", Directory("foo") / "..")
    testParent(Path("foo") / ".." / "." / "bar", Directory("foo") / ".." / ".")

    testParent(Path("foo.txt"), Directory("."))
    testParent(Path(".") / "foo.txt", Directory("."))
    testParent(Path(".") / "baz" / "bar" / "foo.txt",
      Directory(".") / "baz" / "bar",
      Directory("baz") / "bar")

    for (root <- Path.roots) {
      testParent(root, root)
      testParent(root / ".", root)
      testParent(root / "..", root / ".." / "..")
      testParent(root / "foo" / ".", root)
      testParent(root / "foo.txt", root)
      testParent(root / "baz" / "bar" / "foo.txt", root / "baz" / "bar")
      testParent(root / "foo" / "bar" / "..",  root / "foo" / "bar" / ".." / "..")
    }
  }

  /** The parent of a path may have multiple valid non-canonical representations.
   *  Here we test that the parent of the specified path is among a curated list
   *  of representations we consider to be valid.
   */
  private def testParent(path: Path, expected: Path*): Unit = {
    val actual = path.parent
    val some = if (expected.length > 1) " one of" else ""
    assert(expected.contains(actual),
      s"""expected$some: ${expected.mkString("<",">, <",">")} but was: <$actual>""")
  }
}
