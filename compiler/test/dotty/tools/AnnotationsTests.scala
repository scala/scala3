package dotty.tools

import vulpix.TestConfiguration

import org.junit.Test

import dotc.ast.Trees.*
import dotc.ast.tpd
import dotc.core.Decorators.*
import dotc.core.Contexts.*
import dotc.core.Constants.*
import dotc.core.Phases.*
import dotc.core.Types.*
import dotc.core.Symbols.*

import java.io.File
import java.nio.file.*

class AnnotationsTest:
  def augmentedClassPath(path: Path) = s"${path}${File.pathSeparator}${TestConfiguration.basicClasspath}"

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

  @Test def hasNativeAnnot: Unit =
    inCompilerContext(TestConfiguration.basicClasspath) {
      val term: TermSymbol = requiredClass("java.lang.invoke.MethodHandle").requiredMethod("invokeExact")
      assert(term.hasAnnotation(defn.NativeAnnot), i"${term.annotations}")
    }

  @Test def `inlined annot arg is retrieved`: Unit =
    val javaSources =
      VirtualJavaSource("Param.java",
        """|import java.lang.annotation.*;
           |@Inherited @Target({ElementType.FIELD}) @Retention(RetentionPolicy.RUNTIME)
           |public @interface Param { String[] value(); }""".stripMargin)
      :: Nil
    val scalaSources =
      """object Bar { inline transparent def bar() = Array("a", "b", "c") }"""
      :: "@Param(Bar.bar()) class Foo"
      :: Nil
    withJavaCompiled(javaSources*): javaOutputDir =>
      inCompilerContext(augmentedClassPath(javaOutputDir), scalaSources = scalaSources*):
        val cls = requiredClass("Foo")
        val param = requiredClass("Param")
        val annots = cls.annotations
        assert(annots.size == 2, i"$annots") // includes SourceFile
        val annot = annots.find(_.symbol == param).getOrElse(assert(false, s"Missing $param"))
        assert(annot.arguments.size == 1, i"${annot.arguments}")
        extension (t: tpd.Tree)
          def stripNamedArg = tpd.stripNamedArg(t)
          def stripInlined = tpd.stripInlined(t)
        annot.arguments(0).stripNamedArg.stripInlined match
        case Apply(Apply(_, List(Typed(SeqLiteral(ks, _), _))), _) =>
          assert(ks.length == 3, i"$ks")
          assert(ks.map(_.tpe).collect { case ConstantType(Constant(s: String)) => s } == List("a", "b", "c"))
        case bad => assert(false, s"Bad arg $bad")
