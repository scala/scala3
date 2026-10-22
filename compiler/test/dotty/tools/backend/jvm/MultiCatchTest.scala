package dotty.tools
package backend.jvm

import dotty.DottyBytecodeTest
import io.*
import org.junit.Test
import org.junit.Assert.*
import org.objectweb.asm.Opcodes.INSTANCEOF
import org.objectweb.asm.tree.ClassNode


class MultiCatchTest extends DottyBytecodeTest {
  import dotty.AsmConverters.*
  @Test
  def nameless = {
    val source = """
    class X {
      def test() =
        try
          throw new Exception()
        catch
          case _: (A | B) => System.out.println("A|B")
          case _ => ()
    }
    """.stripMargin
    val javaSource = """
      class J {
        public J() {
        }

        public void test() {
          try {
            throw new Exception();
          } catch (A | B ignored) {
            System.out.println("A|B");
          } catch (Throwable ignored) {
          }
        }
      }
      """
    withJavaEquivalentCompiled("J.java",javaSource){
      (dir,sharedJavaSources) =>
        val clsIn = lookupClass(dir, "J.class")
        val clsNode = loadClassNode(clsIn)
        val methJava   = getMethod(clsNode, "test")
        val handlers = convertMethod(methJava).handlers
        testHandlersInvariant(handlers)

        checkBCode(source:: Nil, sharedJavaSources){dir =>
          val clsIn   = lookupClass(dir, "X.class")
          val clsNode = loadClassNode(clsIn)
          val methScala   = getMethod(clsNode, "test")
          val handlers = convertMethod(methScala).handlers
          testHandlersInvariant(handlers)
        }
    }
  }

  @Test
  def bound = {
    val source = """
    class X {
      def test() =
        try
          throw new Exception()
        catch
          case x: (A | B) => println(x)
          case _ => ()
    }
    """.stripMargin
    val javaSource = """
      class J {
        public J() {
        }

        public void test() {
          try {
            throw new Exception();
          } catch (A | B x) {
            System.out.println(x);
          } catch (Throwable ignored) {
          }
        }
      }
    """
    withJavaEquivalentCompiled("J.java",javaSource){
      (dir,sharedJavaSources) =>
        val clsIn = lookupClass(dir, "J.class")
        val clsNode = loadClassNode(clsIn)
        val methJava   = getMethod(clsNode, "test")
        val handlers = convertMethod(methJava).handlers
        testHandlersInvariant(handlers)
        checkBCode(source::Nil, sharedJavaSources){dir =>
          val clsIn   = lookupClass(dir, "X.class")
          val clsNode = loadClassNode(clsIn)
          val meth   = getMethod(clsNode, "test")
          val instructions = instructionsFromMethod(meth)
          val handlers = convertMethod(meth).handlers
          testHandlersInvariant(handlers)
        }
    }
  }

  @Test
  def leadingUnapplyCase = {
    val source = """
    case class Ex() extends Exception
    class Z:
      def test() =
        try {
          throw Ex();
        } catch {
          case Ex() => System.out.println("OK");
          case _: Exception => System.out.println("should be unreachable");
        }
     """
    checkBCode(source){dir =>
      val clsIn = lookupClass(dir, "Z.class")
      val clsNode = loadClassNode(clsIn)
      val meth   = getMethod(clsNode, "test")
      val instructions = instructionsFromMethod(meth)
      val handlers = convertMethod(meth).handlers
      // catch cases are converted into a single catch block
      assertEquals(1, handlers.size)
      val catchBlock = meth.tryCatchBlocks.get(0)
      assertTrue(hasInstanceOfInsideBlock(catchBlock, "Ex"))
      assertTrue(hasInstanceOfInsideBlock(catchBlock, "java/lang/Exception"))
    }
  }

  private def withJavaEquivalentCompiled(
    name: String, javaSource: String
  )(f: (AbstractFile, List[String]) => Unit) =
    val sharedJavaSources = List(
      VirtualJavaSource("A.java", "class A extends Exception {}"),
      VirtualJavaSource("B.java", "class B extends Exception {}"),
    )
    withJavaCompiled((sharedJavaSources :+ VirtualJavaSource(name, javaSource))*): dir =>
      f(PlainDirectory(Directory(dir)), sharedJavaSources.map(_.getCharContent(ignoreEncodingErrors = true)))
  private def testHandlersInvariant(handlers: List[ExceptionHandler]) =
    // The compiler should emit the precise exception table for multi-catch.
    assertEquals(3, handlers.size)
    assertEquals(Some("A"), handlers(0).desc)
    assertEquals(Some("B"), handlers(1).desc)
    assertEquals(Some("java/lang/Throwable"), handlers(2).desc)
    // A and B should have the same start and end labels, and handler label,
    // since they are part of the same multi-catch block
    assertEquals(handlers(0).start, handlers(1).start)
    assertEquals(handlers(0).end, handlers(1).end)
    assertEquals(handlers(0).handler, handlers(1).handler)
    // The third handler is for Throwable, which is the second catch block.
    // It should have the same start and end labels, but a different handler label.
    assertEquals(handlers(0).start, handlers(2).start)
    assertEquals(handlers(0).end, handlers(2).end)
    assertNotEquals(handlers(0).handler, handlers(2).handler)
}
