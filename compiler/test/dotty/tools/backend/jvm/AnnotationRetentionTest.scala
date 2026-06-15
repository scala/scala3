package dotty.tools.backend.jvm

import dotty.DottyBytecodeTest

import scala.language.unsafeNulls
import org.junit.Assert.*
import org.junit.Test

import scala.tools.asm.tree.AnnotationNode
import scala.jdk.CollectionConverters.*

/** Regression test for scala/scala3#389: the JVM backend must honour the `@Retention`
 *  meta-annotation of a Java-defined annotation when deciding how to emit it.
 *
 *  Since the Java parser now reads meta-annotations on `@interface` declarations, an
 *  explicit `@Retention` is respected, and this test pins that: RUNTIME →
 *  `RuntimeVisibleAnnotations`, CLASS → `RuntimeInvisibleAnnotations`, SOURCE → dropped.
 *  It checks both a class and a method to exercise the class- and method-level emit paths.
 *
 *  A Java annotation that declares no `@Retention` defaults to CLASS in the JLS; dotty
 *  currently emits it as runtime-visible for SI-8926 source-compatibility. That
 *  visible-vs-invisible default is a policy detail this test deliberately does NOT freeze
 *  (see the per-assertion note below) — it only checks that, unlike SOURCE, such an
 *  annotation is retained at all.
 *
 *  Replaces the disabled partest-based `tests/disabled/reflect/run/t4788`.
 */
class AnnotationRetentionTest extends DottyBytecodeTest {

  private val javaSources = List(
    """import java.lang.annotation.Retention;
      |import static java.lang.annotation.RetentionPolicy.RUNTIME;
      |@Retention(RUNTIME) public @interface RuntimeAnn {}
    """.stripMargin,
    """import java.lang.annotation.Retention;
      |import static java.lang.annotation.RetentionPolicy.CLASS;
      |@Retention(CLASS) public @interface ClassAnn {}
    """.stripMargin,
    """import java.lang.annotation.Retention;
      |import static java.lang.annotation.RetentionPolicy.SOURCE;
      |@Retention(SOURCE) public @interface SourceAnn {}
    """.stripMargin,
    """public @interface DefaultAnn {}
    """.stripMargin,
  )

  private val scalaSource =
    """@RuntimeAnn @ClassAnn @SourceAnn @DefaultAnn
      |class Annotated:
      |  @RuntimeAnn @ClassAnn @SourceAnn def method(): Unit = ()
    """.stripMargin

  /** Descriptors of the annotations in the given (possibly null) ASM list. */
  private def descriptorsOf(annots: java.util.List[AnnotationNode]): Set[String] =
    Option(annots).fold(Set.empty[String])(_.asScala.map(_.desc).toSet)

  @Test def javaAnnotationRetention: Unit =
    checkBCode(List(scalaSource), javaSources) { dir =>
      val annotated = findClass("Annotated", dir)
      val visible   = descriptorsOf(annotated.visibleAnnotations)
      val invisible = descriptorsOf(annotated.invisibleAnnotations)

      // @Retention(RUNTIME) → emitted as RuntimeVisibleAnnotations
      assertTrue("RuntimeAnn should be runtime-visible", visible.contains("LRuntimeAnn;"))
      assertFalse("RuntimeAnn should not be invisible", invisible.contains("LRuntimeAnn;"))

      // @Retention(CLASS) → emitted as RuntimeInvisibleAnnotations
      assertTrue("ClassAnn should be runtime-invisible", invisible.contains("LClassAnn;"))
      assertFalse("ClassAnn should not be visible", visible.contains("LClassAnn;"))

      // @Retention(SOURCE) → not emitted at all
      assertFalse("SourceAnn should be dropped (visible)", visible.contains("LSourceAnn;"))
      assertFalse("SourceAnn should be dropped (invisible)", invisible.contains("LSourceAnn;"))

      // No @Retention → the JLS defaults to CLASS, but for SI-8926 source-compatibility dotty
      // currently emits it as RUNTIME-visible. That visible-vs-invisible choice is a contested
      // policy, so we deliberately do NOT pin it here. We only assert the uncontroversial part:
      // unlike SOURCE, a no-@Retention annotation is retained in the classfile (some bucket).
      val defaultRetained = visible.contains("LDefaultAnn;") || invisible.contains("LDefaultAnn;")
      assertTrue("DefaultAnn should be retained in bytecode (unlike SOURCE)", defaultRetained)

      // Same retention decision applies to the method-level emit path (visitAnnotation on a
      // MethodVisitor), not just the class. Spot-check the well-defined RUNTIME/CLASS/SOURCE cases.
      val method      = getMethod(annotated, "method")
      val mVisible    = descriptorsOf(method.visibleAnnotations)
      val mInvisible  = descriptorsOf(method.invisibleAnnotations)
      assertTrue("RuntimeAnn should be runtime-visible on method", mVisible.contains("LRuntimeAnn;"))
      assertFalse("RuntimeAnn should not be invisible on method", mInvisible.contains("LRuntimeAnn;"))
      assertTrue("ClassAnn should be runtime-invisible on method", mInvisible.contains("LClassAnn;"))
      assertFalse("ClassAnn should not be visible on method", mVisible.contains("LClassAnn;"))
      assertFalse("SourceAnn should be dropped on method (visible)", mVisible.contains("LSourceAnn;"))
      assertFalse("SourceAnn should be dropped on method (invisible)", mInvisible.contains("LSourceAnn;"))
    }
}
