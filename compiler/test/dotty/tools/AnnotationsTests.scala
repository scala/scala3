package dotty.tools

import vulpix.TestConfiguration

import org.junit.Test

import dotc.ast.Trees._
import dotc.core.Decorators._
import dotc.core.Contexts._
import dotc.core.Types._

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
        val cls = ctx.requiredClass("A")
        val annotCls = ctx.requiredClass("Annot")
        val arrayOfString = defn.ArrayType.appliedTo(List(defn.StringType))

        ctx.atPhase(ctx.erasurePhase.next) {
          val annot = cls.getAnnotation(annotCls)
          // Even though we're forcing the annotation after erasure,
          // the typed trees should be unerased, so the type of
          // the annotation argument should be `arrayOfString` and
          // not a `JavaArrayType`.
          val arg = annot.get.argument(0).get
          assert(arg.tpe.isInstanceOf[AppliedType] && arg.tpe =:= arrayOfString,
            s"Argument $arg had type:\n${arg.tpe}\nbut expected type:\n$arrayOfString")
        }
      }
    }
