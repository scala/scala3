package dotty.tools.languageserver

import org.junit.Test

import dotty.tools.languageserver.util.Code._
import dotty.tools.languageserver.util.CodeRange
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
    testRenameFrom(m3)
    testRenameFrom(m4)
  }

  @Test def renameImport: Unit = {
    def testRenameFrom(m: CodeMarker) =
      withSources(
        code"""object A { class ${m1}C${m2} }""",
        code"""import A.${m3}C${m4}
               object B"""
      ).rename(m, "NewName", Set(m1 to m2, m3 to m4))

    testRenameFrom(m1)
    testRenameFrom(m2)
    testRenameFrom(m3)
    testRenameFrom(m4)
  }

  @Test def renameRenamedImport: Unit = {
    def sources =
      withSources(
        code"""object A { class ${m1}C${m2} }""",
        code"""import A.{${m3}C${m4} => ${m5}D${m6}}
               object B { new ${m7}D${m8} }"""
      )
    def testRename(m: CodeMarker, expectations: Set[CodeRange]) =
      sources.rename(m, "NewName", expectations)

    testRename(m1, Set(m1 to m2, m3 to m4))
    testRename(m2, Set(m1 to m2, m3 to m4))
    testRename(m3, Set(m1 to m2, m3 to m4))
    testRename(m4, Set(m1 to m2, m3 to m4))
    testRename(m5, Set(m5 to m6, m7 to m8))
    testRename(m6, Set(m5 to m6, m7 to m8))
    testRename(m7, Set(m5 to m6, m7 to m8))
    testRename(m8, Set(m5 to m6, m7 to m8))
  }

  @Test def renameRenamingImport: Unit = {
    def sources =
      withSources(
        code"""object A { class ${m1}C${m2}; object ${m3}C${m4} }""",
        code"""object O1 {
                 import A.{${m5}C${m6} => ${m7}Renamed${m8}}
                 class C2 extends ${m9}Renamed${m10} { val x = ${m11}Renamed${m12} }
               }
               object O2 {
                 import A.{${m13}C${m14} => ${m15}Renamed${m16}}
                 class C3 extends ${m17}Renamed${m18} { val x = ${m19}Renamed${m20} }
               }"""
      )
    def testRename(m: CodeMarker, expectations: Set[CodeRange]) =
      sources.rename(m, "NewName", expectations)

    testRename(m1, Set(m1 to m2, m3 to m4, m5 to m6, m13 to m14))
    testRename(m2, Set(m1 to m2, m3 to m4, m5 to m6, m13 to m14))
    testRename(m3, Set(m1 to m2, m3 to m4, m5 to m6, m13 to m14))
    testRename(m4, Set(m1 to m2, m3 to m4, m5 to m6, m13 to m14))
    testRename(m5, Set(m1 to m2, m3 to m4, m5 to m6, m13 to m14))
    testRename(m6, Set(m1 to m2, m3 to m4, m5 to m6, m13 to m14))

    testRename(m7, Set(m7 to m8, m9 to m10, m11 to m12, m15 to m16, m17 to m18, m19 to m20))
    testRename(m8, Set(m7 to m8, m9 to m10, m11 to m12, m15 to m16, m17 to m18, m19 to m20))
    testRename(m9, Set(m7 to m8, m9 to m10, m11 to m12, m15 to m16, m17 to m18, m19 to m20))
    testRename(m10, Set(m7 to m8, m9 to m10, m11 to m12, m15 to m16, m17 to m18, m19 to m20))
    testRename(m11, Set(m7 to m8, m9 to m10, m11 to m12, m15 to m16, m17 to m18, m19 to m20))
    testRename(m12, Set(m7 to m8, m9 to m10, m11 to m12, m15 to m16, m17 to m18, m19 to m20))

    testRename(m13, Set(m1 to m2, m3 to m4, m5 to m6, m13 to m14))
    testRename(m14, Set(m1 to m2, m3 to m4, m5 to m6, m13 to m14))

    testRename(m15, Set(m7 to m8, m9 to m10, m11 to m12, m15 to m16, m17 to m18, m19 to m20))
    testRename(m16, Set(m7 to m8, m9 to m10, m11 to m12, m15 to m16, m17 to m18, m19 to m20))
    testRename(m17, Set(m7 to m8, m9 to m10, m11 to m12, m15 to m16, m17 to m18, m19 to m20))
    testRename(m18, Set(m7 to m8, m9 to m10, m11 to m12, m15 to m16, m17 to m18, m19 to m20))
    testRename(m19, Set(m7 to m8, m9 to m10, m11 to m12, m15 to m16, m17 to m18, m19 to m20))
    testRename(m20, Set(m7 to m8, m9 to m10, m11 to m12, m15 to m16, m17 to m18, m19 to m20))

  }

  @Test def renameRenamingImportNested: Unit = {
    def sources =
      withSources(
        code"""object A { class C }""",
        code"""import A.{C => ${m1}Renamed${m2}}
               object O {
                 import A.{C => ${m3}Renamed${m4}}
                 class C2 extends ${m5}Renamed${m6} { self: ${m15}Renamed${m16} =>
                   import A.{C => ${m7}Renamed${m8}}
                 }
                 123 match {
                   case x if new ${m9}Renamed${m10} == null => ???
                   case foo if {
                     import A.{C => ${m11}Renamed${m12}}
                     new ${m13}Renamed${m14} != null
                   } => ???
                 }
                 new A.C
               }"""
      )
    def testRename(m: CodeMarker, expectations: Set[CodeRange]) =
      sources.rename(m, "NewName", expectations)

    testRename(m1, Set(m1 to m2, m3 to m4, m5 to m6, m7 to m8, m9 to m10, m11 to m12, m13 to m14, m15 to m16))
    testRename(m2, Set(m1 to m2, m3 to m4, m5 to m6, m7 to m8, m9 to m10, m11 to m12, m13 to m14, m15 to m16))
    testRename(m3, Set(m1 to m2, m3 to m4, m5 to m6, m7 to m8, m9 to m10, m11 to m12, m13 to m14, m15 to m16))
    testRename(m4, Set(m1 to m2, m3 to m4, m5 to m6, m7 to m8, m9 to m10, m11 to m12, m13 to m14, m15 to m16))
    testRename(m5, Set(m1 to m2, m3 to m4, m5 to m6, m7 to m8, m9 to m10, m11 to m12, m13 to m14, m15 to m16))
    testRename(m6, Set(m1 to m2, m3 to m4, m5 to m6, m7 to m8, m9 to m10, m11 to m12, m13 to m14, m15 to m16))
    testRename(m7, Set(m1 to m2, m3 to m4, m5 to m6, m7 to m8, m9 to m10, m11 to m12, m13 to m14, m15 to m16))
    testRename(m8, Set(m1 to m2, m3 to m4, m5 to m6, m7 to m8, m9 to m10, m11 to m12, m13 to m14, m15 to m16))
    testRename(m9, Set(m1 to m2, m3 to m4, m5 to m6, m7 to m8, m9 to m10, m11 to m12, m13 to m14, m15 to m16))
    testRename(m10, Set(m1 to m2, m3 to m4, m5 to m6, m7 to m8, m9 to m10, m11 to m12, m13 to m14, m15 to m16))
    testRename(m11, Set(m1 to m2, m3 to m4, m5 to m6, m7 to m8, m9 to m10, m11 to m12, m13 to m14, m15 to m16))
    testRename(m12, Set(m1 to m2, m3 to m4, m5 to m6, m7 to m8, m9 to m10, m11 to m12, m13 to m14, m15 to m16))
    testRename(m13, Set(m1 to m2, m3 to m4, m5 to m6, m7 to m8, m9 to m10, m11 to m12, m13 to m14, m15 to m16))
    testRename(m14, Set(m1 to m2, m3 to m4, m5 to m6, m7 to m8, m9 to m10, m11 to m12, m13 to m14, m15 to m16))
    testRename(m15, Set(m1 to m2, m3 to m4, m5 to m6, m7 to m8, m9 to m10, m11 to m12, m13 to m14, m15 to m16))
    testRename(m16, Set(m1 to m2, m3 to m4, m5 to m6, m7 to m8, m9 to m10, m11 to m12, m13 to m14, m15 to m16))
  }

  @Test def renameImportWithRenaming: Unit = {
    def testRename(m: CodeMarker) =
      withSources(
        code"""object A { class ${m1}C${m2} }""",
        code"""import A.${m3}C${m4}
               object O {
                 class B extends ${m5}C${m6} {
                   import A.{${m7}C${m8} => Renamed}
                   def foo = new Renamed
                 }
               }"""
      ).rename(m, "NewName", Set(m1 to m2, m3 to m4, m5 to m6, m7 to m8))

    testRename(m1)
    testRename(m2)
    testRename(m3)
    testRename(m4)
    testRename(m5)
    testRename(m6)
    testRename(m7)
    testRename(m8)
  }

  @Test def renameDifferentProject: Unit = {
    def testRenameFrom(m: CodeMarker) = {
      val p0 = Project.withSources(
        code"""object A { class ${m1}C${m2} }"""
      )

      val p1 = Project.dependingOn(p0).withSources(
        code"""class B extends A.${m3}C${m4}"""
      )

      val p2 = Project.dependingOn(p0, p1).withSources(
        code"""object D { val x: A.${m5}C${m6} = new A.${m7}C${m8} }"""
      )

      withProjects(p0, p1, p2).rename(m, "NewName", Set(m1 to m2, m3 to m4, m5 to m6, m7 to m8))
    }

    testRenameFrom(m1)
    testRenameFrom(m2)
    testRenameFrom(m3)
    testRenameFrom(m4)
    testRenameFrom(m5)
    testRenameFrom(m6)
    testRenameFrom(m7)
    testRenameFrom(m8)
  }

  @Test def renameDifferentProjectUntouched: Unit = {
    def testRenameFrom(m: CodeMarker) = {
      val p0 = Project.withSources(
        tasty"""object A { class ${m1}C${m2} }"""
      )

      val p1 = Project.dependingOn(p0).withSources(
        code"""class B extends A.${m3}C${m4}"""
      )

      val p2 = Project.dependingOn(p0, p1).withSources(
        tasty"""object D { val x: A.${m5}C${m6} = new A.${m7}C${m8} }"""
      )

      withProjects(p0, p1, p2).rename(m, "NewName", Set(m1 to m2, m3 to m4, m5 to m6, m7 to m8))
    }

    testRenameFrom(m3)
    testRenameFrom(m4)
  }


}
