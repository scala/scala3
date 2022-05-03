package dotty.tools.languageserver

import org.junit.Test

import dotty.tools.languageserver.util.Code._
import dotty.tools.languageserver.util.CodeRange
import dotty.tools.languageserver.util.embedded.CodeMarker

class RenameTest {

  @Test def rename0: Unit = {
    def testRenameFrom(m: CodeMarker) =
      code"class ${m1}Foo$m2 { new ${m3}Foo$m4 }".rename(m, "Bar", Set(m1 to m2, m3 to m4))
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

  /*@Test*/ def renameRenamedImport: Unit = {
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

  /*@Test*/ def renameRenamingImport: Unit = {
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

  /*@Test*/ def renameRenamingImportNested: Unit = {
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

  @Test def renameOverridden: Unit = {
    def testRename(m: CodeMarker, expectations: Set[CodeRange], withOverridden: Option[Boolean]) =
      withSources(
        code"""class A { def ${m1}foo${m2}: Int = 0 }
               class B extends A { override def ${m3}foo${m4}: Int = 1 }
               class C extends A { override def ${m5}foo${m6}: Int = 2 }"""
      ).rename(m, "NewName", expectations, withOverridden)

    testRename(m1, Set(m1 to m2, m3 to m4, m5 to m6), withOverridden = None)
    testRename(m2, Set(m1 to m2, m3 to m4, m5 to m6), withOverridden = None)
    testRename(m3, Set(m1 to m2, m3 to m4, m5 to m6), withOverridden = Some(true))
    testRename(m4, Set(m1 to m2, m3 to m4, m5 to m6), withOverridden = Some(true))
    testRename(m5, Set(m1 to m2, m3 to m4, m5 to m6), withOverridden = Some(true))
    testRename(m6, Set(m1 to m2, m3 to m4, m5 to m6), withOverridden = Some(true))
    testRename(m3, Set(m3 to m4), withOverridden = Some(false))
    testRename(m4, Set(m3 to m4), withOverridden = Some(false))
    testRename(m5, Set(m5 to m6), withOverridden = Some(false))
    testRename(m6, Set(m5 to m6), withOverridden = Some(false))

  }

  @Test def renameImportFromTasty: Unit = {
    // Note that everything here is in the empty package; this ensures that we will
    // use the sourcefile loader to load `class Bar`.
    def testRename(m: CodeMarker) = {
      withSources(
        code"""object O { class ${m1}Foo${m2} }""",
        tasty"""import O.${m3}Foo${m4}
                class Bar extends ${m5}Foo${m6}"""
      ).rename(m, "NewName", Set(m1 to m2, m3 to m4, m5 to m6))
    }

    testRename(m1)
    testRename(m2)
  }

  /*@Test*/ def renameValMultiProject: Unit = {
    def testRename(m: CodeMarker, expectations: Set[CodeRange]) = {
      val p0 = Project.withSources(
        code"""object A { val ${m1}foo${m2} = 0 }"""
      )

      val p1 = Project.dependingOn(p0).withSources(
        code"""object B { val ${m3}bar${m4} = A.${m5}foo${m6} }"""
      )

      val p2 = Project.dependingOn(p1).withSources(
        code"""object C { val ${m7}baz${m8} = A.${m9}foo${m10} + B.${m11}bar${m12} }"""
      )

      withProjects(p0, p1, p2).rename(m, "NewName", expectations)
    }

    testRename(m1, Set(m1 to m2, m5 to m6, m9 to m10))
    testRename(m5, Set(m1 to m2, m5 to m6, m9 to m10))
    testRename(m9, Set(m1 to m2, m5 to m6, m9 to m10))

    testRename(m3, Set(m3 to m4, m11 to m12))
    testRename(m11, Set(m3 to m4, m11 to m12))

    testRename(m7, Set(m7 to m8))
  }

  /*@Test*/ def renameClassMultiProject: Unit = {
    val m21 = new CodeMarker("m21")
    val m22 = new CodeMarker("m22")
    val m23 = new CodeMarker("m23")
    val m24 = new CodeMarker("m24")
    val m25 = new CodeMarker("m25")
    val m26 = new CodeMarker("m26")
    val m27 = new CodeMarker("m27")
    val m28 = new CodeMarker("m28")
    def testRename(m: CodeMarker, expectations: Set[CodeRange]) = {
      val p0 = Project.withSources(
        code"""package a
               object ${m1}A${m2} { class ${m3}B${m4} }"""
      )

      val p1 = Project.dependingOn(p0).withSources(
        code"""package b
               import a.${m5}A${m6}.{${m7}B${m8} => ${m9}AB${m10}}
               object ${m11}B${m12} { class ${m13}C${m14} extends ${m15}AB${m16} }"""
      )

      val p2 = Project.dependingOn(p1).withSources(
        code"""package c
               import b.${m17}B${m18}.{${m19}C${m20} => ${m21}BC${m22}}
               object ${m23}C${m24} { class ${m25}D${m26} extends ${m27}BC${m28} }"""
      )

      withProjects(p0, p1, p2).rename(m, "NewName", expectations)
    }

    testRename(m1, Set(m1 to m2, m5 to m6))
    testRename(m5, Set(m1 to m2, m5 to m6))

    testRename(m3, Set(m3 to m4, m7 to m8))
    testRename(m7, Set(m3 to m4, m7 to m8))

    testRename(m9, Set(m9 to m10, m15 to m16))
    testRename(m15, Set(m9 to m10, m15 to m16))

    testRename(m11, Set(m11 to m12, m17 to m18))
    testRename(m17, Set(m11 to m12, m17 to m18))

    testRename(m13, Set(m13 to m14, m19 to m20))
    testRename(m19, Set(m13 to m14, m19 to m20))

    testRename(m21, Set(m21 to m22, m27 to m28))
    testRename(m27, Set(m21 to m22, m27 to m28))

    testRename(m23, Set(m23 to m24))

    testRename(m25, Set(m25 to m26))

  }

}
