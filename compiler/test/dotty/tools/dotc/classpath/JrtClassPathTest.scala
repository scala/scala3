package dotty.tools.dotc.classpath

import org.junit.Assert.*
import org.junit.Test

import org.objectweb.asm.ClassReader
import org.objectweb.asm.tree.ClassNode

class JrtClassPathTest {

  @Test def lookupJavaClasses(): Unit = {
    val cp: ClassPath = JrtClassPath(None).get

    assertTrue(cp.classes("").isEmpty)
    assertTrue(cp.packages("java").toString, cp.packages("java").exists(_ == "java.lang"))
    assertTrue(cp.classes("java.lang").exists(_.name == "Object"))
    val jl_Object = cp.classes("java.lang").find(_.name == "Object").get
    val jl_Class = {
      val node = new ClassNode()
      new ClassReader(jl_Object.file.toByteArray).accept(node, 0)
      node
    }
    assertEquals("java/lang/Object", jl_Class.name)
    assertTrue(cp.packages("java.lang").exists(_ == "java.lang.annotation"))
    assertTrue(cp.findClassFile("java.lang.Object").isDefined)
  }
}
