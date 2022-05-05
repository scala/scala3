package dotty.tools

import vulpix.TestConfiguration

import org.junit.Test

import dotc.ast.Trees._
import dotc.core.Decorators._
import dotc.core.Contexts._
import dotc.core.Phases._
import dotc.core.Types._
import dotc.core.Symbols._

import java.io.File
import java.nio.file._

class AnnotationsTest:

  @Test def annotTreeNotErased: Unit =
    withJavaCompiled(
      VirtualJavaSource("Annot.java",
        "public @interface Annot { String[] values() default {}; }"),
      VirtualJavaSource("A.java",
        "@Annot(values = {}) public class A {}")) { javaOutputDir =>
      inCompilerContext(javaOutputDir.toString + File.pathSeparator + TestConfiguration.basicClasspath) {
        val defn = ctx.definitions
        val cls = requiredClass("A")
        val annotCls = requiredClass("Annot")
        val arrayOfString = defn.ArrayType.appliedTo(List(defn.StringType))

        atPhase(erasurePhase.next) {
          // Even though we're forcing the annotation after erasure,
          // the typed trees should be unerased, so the type of
          // the annotation argument should be `arrayOfString` and
          // not a `JavaArrayType`.
          val annot = cls.getAnnotation(annotCls)
          val arg = annot.get.argument(0).get

          // If we run the type check after erasure, we will have
          // `Array[String] =:= Array[String]` being false.
          // The reason is that in `TypeComparer.compareAppliedType2` we have
          // `tycon2.typeParams == Nil` after erasure, thus always get false.
          val res = atPhase(typerPhase) { arrayOfString =:= arg.tpe }

          assert(arg.tpe.isInstanceOf[AppliedType] && res,
            s"Argument $arg had type:\n${arg.tpe}\nbut expected type:\n$arrayOfString")
        }
      }
    }

  @Test def surviveMissingAnnot: Unit =
    withJavaCompiled(
      VirtualJavaSource("Annot1.java",
        "public @interface Annot1 {}"),
      VirtualJavaSource("Annot2.java",
        "public @interface Annot2 {}"),
      VirtualJavaSource("A.java",
        "@Annot1() @Annot2() public class A {}")) { javaOutputDir =>
      Files.delete(javaOutputDir.resolve("Annot1.class"))
      inCompilerContext(javaOutputDir.toString + File.pathSeparator + TestConfiguration.basicClasspath) {
        val cls = requiredClass("A")
        val annots = cls.annotations.map(_.tree)
        assert(annots.length == 1,
          s"class A should have only one visible annotation since Annot is not on the classpath, but found: $annots")
        assert(!ctx.reporter.hasErrors && !ctx.reporter.hasWarnings,
          s"A missing annotation while parsing a Java class should be silently ignored but: ${ctx.reporter.summary}")
      }
    }

  @Test def surviveMissingInnerClassAnnot: Unit =
    withJavaCompiled(
      VirtualJavaSource("Outer.java",
        """|package a.b;
           |public @interface Outer { public @interface Value { @interface Immutable {} } }
           |""".stripMargin),
      VirtualJavaSource("Baz.java",
        """|package a.b;
           |@Outer.Value.Immutable abstract class Baz {}""".stripMargin)
    ) { javaOutputDir =>
      Files.delete(javaOutputDir.resolve("a/b/Outer.class"))
      Files.delete(javaOutputDir.resolve("a/b/Outer$Value.class"))
      Files.delete(javaOutputDir.resolve("a/b/Outer$Value$Immutable.class"))
      inCompilerContext(javaOutputDir.toString + File.pathSeparator + TestConfiguration.basicClasspath) {
        val cls = requiredClass("a.b.Baz")
        val annots = cls.annotations.map(_.tree)
        assert(annots == Nil,
          s"class Baz should have no visible annotations since Outer.Value.Immutable is not on the classpath, but found: $annots")
        assert(!ctx.reporter.hasErrors && !ctx.reporter.hasWarnings,
          s"A missing annotation while parsing a Java class should be silently ignored but: ${ctx.reporter.summary}")
      }
    }
