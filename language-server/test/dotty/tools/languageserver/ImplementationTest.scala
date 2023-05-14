package dotty.tools.languageserver

import dotty.tools.languageserver.util.Code._

import org.junit.Test

class ImplementationTest {

  @Test def implMethodFromTrait: Unit = {
    code"""trait A {
             def ${m1}foo${m2}(x: Int): String
           }
           class B extends A {
             override def ${m3}foo${m4}(x: Int): String = ""
           }"""
      .implementation(m1 to m2, List(m3 to m4))
      .implementation(m3 to m4, List(m3 to m4))
  }

  @Test def implMethodFromTrait0: Unit = {
    code"""trait A {
             def ${m1}foo${m2}(x: Int): String
           }
           class B extends A {
             override def ${m3}foo${m4}(x: Int): String = ""
           }
           class C extends B {
             override def ${m5}foo${m6}(x: Int): String = ""
           }"""
      .implementation(m1 to m2, List(m3 to m4, m5 to m6))
      .implementation(m3 to m4, List(m3 to m4, m5 to m6))
      .implementation(m5 to m6, List(m5 to m6))
  }

  @Test def extendsTrait: Unit = {
    code"""trait ${m1}A${m2}
           class ${m3}B${m4} extends ${m5}A${m6}"""
      .implementation(m1 to m2, List(m3 to m4))
      .implementation(m3 to m4, List(m3 to m4))
      .implementation(m5 to m6, List(m3 to m4))
  }

  @Test def extendsClass: Unit = {
    code"""class ${m1}A${m2}
           class ${m3}B${m4} extends ${m5}A${m6}"""
      .implementation(m1 to m2, List(m1 to m2, m3 to m4))
      .implementation(m3 to m4, List(m3 to m4))
      .implementation(m5 to m6, List(m1 to m2, m3 to m4))
  }

  @Test def objExtendsTrait: Unit = {
    code"""trait ${m1}A${m2}
           object ${m3}B${m4} extends ${m5}A${m6}"""
      .implementation(m1 to m2, List(m3 to m4))
      .implementation(m3 to m4, List(m3 to m4))
      .implementation(m5 to m6, List(m3 to m4))
  }

  @Test def defineAbstractType: Unit = {
    code"""trait A { type ${m1}T${m2} }
           trait B extends A { type ${m3}T${m4} = Int }"""
      .implementation(m1 to m2, List(m3 to m4))
      .implementation(m3 to m4, List(m3 to m4))
  }

  @Test def innerClass: Unit = {
    code"""trait A { trait ${m1}AA${m2} }
           class B extends A {
             class ${m3}AB${m4} extends ${m5}AA${m6}
           }"""
      .implementation(m1 to m2, List(m3 to m4))
      .implementation(m3 to m4, List(m3 to m4))
      .implementation(m5 to m6, List(m3 to m4))
  }

}
