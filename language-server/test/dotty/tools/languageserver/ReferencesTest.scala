package dotty.tools.languageserver

import org.junit.Test

import dotty.tools.languageserver.util.Code._

class ReferencesTest {

  @Test def valNoReferences0: Unit =
    code"class X { val ${m1}x$m2 = 9 }".withSource.references(m1 to m2, Nil)

  @Test def valReferences0: Unit = {
    code"class X { val ${m1}x$m2 = 9; ${m3}x$m4; ${m5}x$m6 }".withSource
      .references(m1 to m2, List(m3 to m4, m5 to m6))
  }

  @Test def valReferences1: Unit = {
    code"class X { val ${m1}x$m2 = 9; ${m3}x$m4; ${m5}x$m6 }".withSource
      .references(m1 to m2, List(m1 to m2, m3 to m4, m5 to m6), withDecl = true)
  }

  @Test def classReference0: Unit = {
    code"class ${m1}Foo${m2} { val a = new ${m3}Foo${m4} }".withSource
      .references(m1 to m2, List(m1 to m2, m3 to m4), withDecl = true)
      .references(m1 to m2, List(m3 to m4), withDecl = false)
      .references(m3 to m4, List(m1 to m2, m3 to m4), withDecl = true)
      .references(m3 to m4, List(m3 to m4), withDecl = false)
  }

  @Test def classReference1: Unit = {
    code"class ${m1}Foo${m2}(x: Int) { val a = new ${m3}Foo${m4}(1) }".withSource
      .references(m1 to m2, List(m1 to m2, m3 to m4), withDecl = true)
      .references(m1 to m2, List(m3 to m4), withDecl = false)
      .references(m3 to m4, List(m1 to m2, m3 to m4), withDecl = true)
      .references(m3 to m4, List(m3 to m4), withDecl = false)
  }

  @Test def classReferenceCompanion: Unit = {
    code"""class ${m1}Foo${m2}(x: Any)
           object ${m3}Foo${m4} { val bar = new ${m5}Foo${m6}(${m7}Foo${m8}) }""".withSource
      .references(m1 to m2, List(m1 to m2, m5 to m6), withDecl = true)
      .references(m1 to m2, List(m5 to m6), withDecl = false)
      .references(m3 to m4, List(m3 to m4, m7 to m8), withDecl = true)
      .references(m3 to m4, List(m7 to m8), withDecl = false)
      .references(m5 to m6, List(m1 to m2, m5 to m6), withDecl = true)
      .references(m5 to m6, List(m5 to m6), withDecl = false)
      .references(m7 to m8, List(m3 to m4, m7 to m8), withDecl = true)
      .references(m7 to m8, List(m7 to m8), withDecl = false)
  }

  @Test def valReferencesInDifferentProject: Unit = {
    val p0 = Project.withSources(
      code"""object A { val ${m1}x${m2} = 1 }"""
    )

    val p1 = Project.dependingOn(p0).withSources(
      code"""object B { A.${m3}x${m4} }"""
    )

    val p2 = Project.dependingOn(p0).withSources(
      code"""object C { A.${m5}x${m6} }"""
    )

    withProjects(p0, p1, p2)
      .references(m1 to m2, List(m1 to m2, m3 to m4, m5 to m6), withDecl = true)
      .references(m1 to m2, List(m3 to m4, m5 to m6), withDecl = false)
      .references(m3 to m4, List(m1 to m2, m3 to m4, m5 to m6), withDecl = true)
      .references(m3 to m4, List(m3 to m4, m5 to m6), withDecl = false)
      .references(m5 to m6, List(m1 to m2, m3 to m4, m5 to m6), withDecl = true)
      .references(m5 to m6, List(m3 to m4, m5 to m6), withDecl = false)
  }

  @Test def valReferencesInDifferentProjectNoDef: Unit = {
    val p0 = Project.withSources(
      code"""object A { new java.util.${m1}ArrayList${m2}[Int] }"""
    )

    val p1 = Project.withSources(
      code"""object B { new java.util.${m3}ArrayList${m4}[Int] }"""
    )

    val p2 = Project.withSources(
      code"""object C { new java.util.${m5}ArrayList${m6}[Int] }"""
    )

    withProjects(p0, p1, p2)
      .references(m1 to m2, List(m1 to m2, m3 to m4, m5 to m6), withDecl = true)
      .references(m1 to m2, List(m1 to m2, m3 to m4, m5 to m6), withDecl = false)
      .references(m3 to m4, List(m1 to m2, m3 to m4, m5 to m6), withDecl = true)
      .references(m3 to m4, List(m1 to m2, m3 to m4, m5 to m6), withDecl = false)
      .references(m5 to m6, List(m1 to m2, m3 to m4, m5 to m6), withDecl = true)
      .references(m5 to m6, List(m1 to m2, m3 to m4, m5 to m6), withDecl = false)
  }

  @Test def moduleReferencesInDifferentProject: Unit = {
    val p0 = Project.withSources(
      code"""object ${m1}A${m2}"""
    )

    val p1 = Project.dependingOn(p0).withSources(
      code"""class B { ${m3}A${m4} }"""
    )

    withProjects(p0, p1)
      .references(m1 to m2, List(m1 to m2, m3 to m4), withDecl = true)
      .references(m1 to m2, List(m3 to m4), withDecl = false)
      .references(m3 to m4, List(m1 to m2, m3 to m4), withDecl = true)
      .references(m3 to m4, List(m3 to m4), withDecl = false)
  }

  @Test def classReferencesInDifferentProject: Unit = {
    val p0 = Project.withSources(
      code"""class ${m1}A${m2}"""
    )

    val p1 = Project.dependingOn(p0).withSources(
      code"""class B extends ${m3}A${m4}"""
    )

    val p2 = Project.dependingOn(p0).withSources(
      code"""class C { new ${m5}A${m6} }"""
    )

    withProjects(p0, p1, p2)
      .references(m1 to m2, List(m1 to m2, m3 to m4, m5 to m6), withDecl = true)
      .references(m1 to m2, List(m3 to m4, m5 to m6), withDecl = false)
      .references(m3 to m4, List(m1 to m2, m3 to m4, m5 to m6), withDecl = true)
      .references(m3 to m4, List(m3 to m4, m5 to m6), withDecl = false)
      .references(m5 to m6, List(m1 to m2, m3 to m4, m5 to m6), withDecl = true)
      .references(m5 to m6, List(m3 to m4, m5 to m6), withDecl = false)
  }

  @Test def defReferencesInDifferentProject: Unit = {
    val p0 = Project.withSources(
      code"""object A { def ${m1}x${m2} = 1 }"""
    )

    val p1 = Project.dependingOn(p0).withSources(
      code"""object B { A.${m3}x${m4} }"""
    )

    val p2 = Project.dependingOn(p0).withSources(
      code"""object C { A.${m5}x${m6} }"""
    )

    withProjects(p0, p1, p2)
      .references(m1 to m2, List(m1 to m2, m3 to m4, m5 to m6), withDecl = true)
      .references(m1 to m2, List(m3 to m4, m5 to m6), withDecl = false)
      .references(m3 to m4, List(m1 to m2, m3 to m4, m5 to m6), withDecl = true)
      .references(m3 to m4, List(m3 to m4, m5 to m6), withDecl = false)
      .references(m5 to m6, List(m1 to m2, m3 to m4, m5 to m6), withDecl = true)
      .references(m5 to m6, List(m3 to m4, m5 to m6), withDecl = false)
  }

  @Test def deeplyNestedValReferencesInDifferentProject: Unit = {
    val p0 = Project.withSources(
      code"""class A { class Z { class Y { class X { val ${m1}x${m2} = 1 } } } }"""
    )

    val p1 = Project.dependingOn(p0).withSources(
      code"""class B {
               val a = new A()
               val z = new a.Z()
               val y = new z.Y()
               val x = new y.X()
               x.${m3}x${m4}
             }"""
    )

    withProjects(p0, p1)
      .references(m1 to m2, List(m1 to m2, m3 to m4), withDecl = true)
      .references(m1 to m2, List(m3 to m4), withDecl = false)
      .references(m3 to m4, List(m1 to m2, m3 to m4), withDecl = true)
      .references(m3 to m4, List(m3 to m4), withDecl = false)
  }

  @Test def deeplyNestedStaticValReferencesInDifferentProject: Unit = {
    val p0 = Project.withSources(
      code"""object A { object Z { object Y { object X { val ${m1}x${m2} = 1 } } } }"""
    )

    val p1 = Project.dependingOn(p0).withSources(
      code"""object B { A.Z.Y.X.${m3}x${m4} }"""
    )

    withProjects(p0, p1)
      .references(m1 to m2, List(m1 to m2, m3 to m4), withDecl = true)
      .references(m1 to m2, List(m3 to m4), withDecl = false)
      .references(m3 to m4, List(m1 to m2, m3 to m4), withDecl = true)
      .references(m3 to m4, List(m3 to m4), withDecl = false)
  }

  @Test def findReferencesInUntouchedProject: Unit = {
    val p0 = Project.withSources(
      code"""package hello
             object A { def ${m1}foo${m2} = 1 }"""
    )

    val p1 = Project.dependingOn(p0).withSources(
      tasty"""package hello
              object B { def bar = A.${m3}foo${m4} }"""
    )

    withProjects(p0, p1)
      .references(m1 to m2, List(m1 to m2, m3 to m4), withDecl = true)
      .references(m1 to m2, List(m3 to m4), withDecl = false)
  }

}
