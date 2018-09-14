package dotty.tools.languageserver

import org.junit.Test

import dotty.tools.languageserver.util.Code._
import dotty.tools.languageserver.util.embedded.CodeMarker

class RenameTest {

  @Test def rename0: Unit = {
    def testRenameFrom(m: CodeMarker) =
      code"class ${m1}Foo$m2 { new ${m3}Foo$m4 }".withSource.rename(m, "Bar", Set(m1 to m2, m3 to m4))
    testRenameFrom(m1)
    testRenameFrom(m3)
  }


  @Test def rename1: Unit = {
    def testRenameFrom(m: CodeMarker) =
      withSources(
        code"class ${m1}Foo$m2 { new ${m3}Foo$m4 }",
        code"class Bar { new ${m5}Foo$m6 }",
        code"class Baz extends ${m7}Foo${m8}"
      ).rename(m, "Bar", Set(m1 to m2, m3 to m4, m5 to m6, m7 to m8))

    testRenameFrom(m1)
    testRenameFrom(m3)
    testRenameFrom(m5)
  }

  @Test def renameObject: Unit = {
    def testRenameFrom(m: CodeMarker) =
      withSources(
        code"object ${m1}Foo${m2}",
        code"class Bar { val x = ${m3}Foo${m4} }"
      ).rename(m, "NewName", Set(m1 to m2, m3 to m4))

    testRenameFrom(m1)
    testRenameFrom(m2)
  }

  @Test def renameDef: Unit = {
    def testRenameFrom(m: CodeMarker) =
      withSources(
        code"object Foo { def ${m1}bar${m2} = 0 }",
        code"object Buzz { Foo.${m3}bar${m4} }"
      ).rename(m, "newName", Set(m1 to m2, m3 to m4))

    testRenameFrom(m1)
    testRenameFrom(m3)
  }

  @Test def renameClass: Unit = {
    def testRenameFrom(m: CodeMarker) =
      withSources(
        code"class ${m1}Foo${m2}(x: Int)",
        code"class Bar extends ${m3}Foo${m4}(1)"
      ).rename(m, "NewName", Set(m1 to m2, m3 to m4))

    testRenameFrom(m1)
    testRenameFrom(m3)
  }

  @Test def renameCaseClass: Unit = {
    def testRenameFrom(m: CodeMarker) =
      withSources(
        code"case class ${m1}Foo${m2}(x: Int)",
        code"class Bar extends ${m3}Foo${m4}(1)"
      ).rename(m, "NewName", Set(m1 to m2, m3 to m4))

    testRenameFrom(m1)
    testRenameFrom(m2)
  }

}
