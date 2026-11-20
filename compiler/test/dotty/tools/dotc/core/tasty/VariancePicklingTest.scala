package dotty.tools.dotc.core.tasty

import dotty.tools.dotc.Main
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.toTypeName
import dotty.tools.dotc.core.Flags.{Contravariant, Covariant}
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.Variances.*
import dotty.tools.dotc.interfaces.Diagnostic.ERROR
import dotty.tools.dotc.reporting.TestReporter
import dotty.tools.dotc.Driver
import dotty.tools.io.{AbstractFile, Directory, File}

import dotty.tools.vulpix.TestConfiguration

import org.junit.Test
import org.junit.Assert.{assertEquals, assertFalse, assertTrue, fail}

/** Tests that the declared variances of the type parameters of a type definition
 *  survive a TASTy round-trip.
 */
class VariancePicklingTest:

  @Test def transparentAlias: Unit =
    // `T` occurs covariantly in the body, so a structural reading would infer
    // `Covariant` here.
    checkDeclaredVariances(
      "object A { type Tr[-T] = List[T @scala.annotation.unchecked.uncheckedVariance] }",
      "Tr", List(Contravariant))

  @Test def abstractTypeMember: Unit =
    checkDeclaredVariances(
      "trait A { type F[-X] <: List[X @scala.annotation.unchecked.uncheckedVariance] }",
      "F", List(Contravariant))

  @Test def severalParams: Unit =
    checkDeclaredVariances("object A { type Fn[B, +C] = B => C }", "Fn", List(Invariant, Covariant))

  @Test def noVarianceAnnotation: Unit =
    // Without any annotation an alias is treated structurally, and no declared
    // variances are recorded at all.
    checkStructural("object A { type Id[T] = List[T] }", "Id")

  private def typeLambdaOf(source: String, name: String): HKTypeLambda =
    compileAndUnpickle(source) { (trees, ctx) =>
      given Context = ctx
      findTypeDef(name.toTypeName)(trees) match
        case Some(tdef) =>
          tdef.symbol.info match
            case bounds: TypeBounds =>
              bounds.hi match
                case tl: HKTypeLambda => tl
                case hi => fail(s"not a type lambda: $hi").asInstanceOf[Nothing]
            case info => fail(s"not type bounds: $info").asInstanceOf[Nothing]
        case None => fail(s"no type definition named $name").asInstanceOf[Nothing]
    }

  private def checkDeclaredVariances(source: String, name: String, expected: List[Variance]): Unit =
    val tl = typeLambdaOf(source, name)
    assertTrue(s"$name was not read back as a declared-variance lambda", tl.isDeclaredVarianceLambda)
    assertEquals(expected.map(varianceToInt), tl.declaredVariances.map(varianceToInt))

  private def checkStructural(source: String, name: String): Unit =
    assertFalse(
      s"$name was unexpectedly read back as a declared-variance lambda",
      typeLambdaOf(source, name).isDeclaredVarianceLambda)

  private def findTypeDef(name: Name)(trees: List[tpd.Tree])(using Context): Option[tpd.TypeDef] =
    trees.flatMap {
      _.find { case tdef: tpd.TypeDef => tdef.name == name case _ => false }
        .map(_.asInstanceOf[tpd.TypeDef]).toList
    }.headOption

  private def compileAndUnpickle[T](source: String)(fn: (List[tpd.Tree], Context) => T): T =
    Directory.inTempDirectory { tmp =>
      val src = tmp./(File("Src.scala")).toAbsolute
      src.writeAll(source)
      val out = tmp./("out")
      out.createDirectory()

      val options = TestConfiguration.defaultOptions
        .and("-d", out.toAbsolute.toString)
        .and(src.toString)
      val reporter = TestReporter.reporter(System.out, logLevel = ERROR)
      Main.process(options.all, reporter)
      assertFalse("Compilation failed.", reporter.hasErrors)

      val tastyFiles = out.walkFilter(f => f.isFile && f.ext.isTasty).map(_.toFile).toList
      val unpicklingOptions = TestConfiguration.defaultOptions
        .withClasspath(out.toAbsolute.toString)
        .and("dummy") // need to pass a dummy source file name
      UnpicklingDriver().unpickle(unpicklingOptions.all, tastyFiles)(fn)
    }

  private class UnpicklingDriver extends Driver:
    def unpickle[T](args: Array[String], files: List[File])(fn: (List[tpd.Tree], Context) => T): T =
      given ctx: Context = setup(args, initCtx).map(_._2).getOrElse(initCtx)
      ctx.initialize()
      val trees = files.flatMap { f =>
        val unpickler = new DottyUnpickler(AbstractFile.getFile(f.jpath).nn, isBestEffortTasty = false)
        unpickler.enter(roots = Set.empty)
        unpickler.rootTrees
      }
      fn(trees, ctx)
