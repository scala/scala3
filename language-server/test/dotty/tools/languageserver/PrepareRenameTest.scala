package dotty.tools.languageserver

import org.junit.Test

import dotty.tools.languageserver.util.Code._

class PrepareRenameTest {

  @Test def canRenameVal: Unit = {
    code"""class Foo {
          |  val ${m1}foo${m2} = 0
          |}""".withSource
      .prepareRename(m1 to m2, success = true)
  }

  @Test def canRenameClass: Unit = {
    code"""class ${m1}Foo${m2}""".withSource
      .prepareRename(m1 to m2, success = true)
  }

  @Test def canRenameObject: Unit = {
    code"""object ${m1}Foo${m2}""".withSource
      .prepareRename(m1 to m2, success = true)
  }

  @Test def canRenameCaseClass: Unit = {
    code"""case class ${m1}Foo${m2}(${m3}x${m4}: Int)""".withSource
      .prepareRename(m1 to m2, success = true)
      .prepareRename(m3 to m4, success = true)
  }

  @Test def canRenameImport: Unit = {
    code"""import ${m1}O${m2}.{${m3}foo${m4} => ${m5}bar${m6}}
          |import O.${m7}foo${m8}
          |object O { def foo = 0 }""".withSource
      .prepareRename(m1 to m2, success = true)
      .prepareRename(m3 to m4, success = true)
      .prepareRename(m5 to m6, success = true)
      .prepareRename(m7 to m8, success = true)
  }

  @Test def cannotRememberSpecialNames: Unit = {
    code"""class Foo {
          |  def ${m1}apply${m2} = ???
          |  def ${m3}unapply${m4} = ???
          |  def ${m5}unapplySeq${m6} = ???
          |}""".withSource
      .prepareRename(m1 to m2, success = false)
      .prepareRename(m3 to m4, success = false)
      .prepareRename(m5 to m6, success = false)
  }

  @Test def cannotRenameSyntheticDef: Unit = {
    code"""case class Foo(x: Int) {
          |  def foo = ${m1}copy${m2}(x = 9)
          |}""".withSource
      .prepareRename(m1 to m2, success = false)
  }

  @Test def cannotRenameIf: Unit = {
    code"""class Foo {
          |  ${m1}if${m2} (true) {
          |    ???
          |  }
          |}""".withSource
      .prepareRename(m1 to m2, success = false)
  }

  @Test def cannotRenameThis: Unit = {
    code"""class Foo {
          |  def bar = ${m1}this${m2}
          |}""".withSource
      .prepareRename(m1 to m2, success = false)
  }

  @Test def cannotRenamePackage: Unit = {
    code"""package ${m1}foo${m2}
          |import ${m3}foo${m4}.Foo
          |class Foo""".withSource
      .prepareRename(m1 to m2, success = false)
      .prepareRename(m3 to m4, success = false)
  }

  @Test def cannotRenameExternalSymbols: Unit = {
    code"""import ${m1}java${m2}.${m3}io${m4}.${m5}FileDescriptor${m6}
          |object Foo {
          |  def foo(x: ${m7}FileDescriptor${m8}) = ???
          |}""".withSource
      .prepareRename(m1 to m2, success = false)
      .prepareRename(m3 to m4, success = false)
      .prepareRename(m5 to m6, success = false)
      .prepareRename(m7 to m8, success = false)
  }

  @Test def canRenameSymbolInExternalProject: Unit = {
    val p0 = Project.withSources(
      code"""package a
             object ${m1}A${m2}"""
    )

    val p1 = Project.dependingOn(p0).withSources(
      code"""package b
             import a.${m3}A${m4}
             object B { val a = ${m5}A${m6} }"""
    )

    withProjects(p0, p1)
      .prepareRename(m1 to m2, success = true)
      .prepareRename(m3 to m4, success = true)
      .prepareRename(m5 to m6, success = true)

  }
}
