package dotty.tools.scaladoc

import org.junit.Assert.assertEquals
import org.junit.Test

class InheritedOverloadsOrderTest extends ScaladocTest("inheritedOverloadsOrder"):

  @Test
  def runTest(): Unit = withModule { module =>
    def collect(m: Member): Seq[Member] = m +: m.members.flatMap(collect)
    val all = collect(module.rootPackage)

    def overloadLines(className: String): List[Int] =
      all.find(_.fullName == s"tests.inheritedOverloadsOrder.$className")
        .toList
        .flatMap(_.members)
        .filter(_.name == "a")
        .flatMap(_.sources.map(_.lineNumber))
        .toList

    val declared = overloadLines("T")
    assertEquals("declared overloads should be in source order", declared.sorted, declared)
    assertEquals("declared overloads should all be found", 3, declared.size)
    assertEquals("inherited overloads should be ordered like declared ones", declared, overloadLines("C"))
  }
