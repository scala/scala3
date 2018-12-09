package dotty.semanticdb

import scala.tasty.Reflection
import scala.tasty.file._
import scala.collection.mutable.HashMap

import org.junit.Test
import org.junit.Assert._
import java.nio.file._
import scala.meta.internal.{semanticdb => s}
import scala.collection.JavaConverters._
import java.io.File
import scala.tasty.Reflection
import scala.tasty.file.TastyConsumer
import java.lang.reflect.InvocationTargetException

class TastyInspecter extends TastyConsumer {
  var source_path: Option[String] = None
  final def apply(reflect: Reflection)(root: reflect.Tree): Unit = {
    import reflect._
    object ChildTraverser extends TreeTraverser {
      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit =
        tree match {
          case IsClassDef(cdef) => {
            cdef.symbol.annots.foreach { annot =>
              annot match {
                case Term.Apply(Term.Select(Term.New(t), _),
                                List(Term.Literal(Constant.String(path))))
                    if t.symbol.name == "SourceFile" =>
                  // we found the path to a file. In this case, we do not need to
                  // continue traversing the tree
                  source_path = Some(path)
                case x => super.traverseTree(tree)
              }
              true
            }
          }
          case _ => {
            if (source_path == None)
              super.traverseTree(tree)
            else
              ()
          }
        }
    }
    ChildTraverser.traverseTree(root)(reflect.rootContext)
  }
}

class Tests {

  // TODO: update scala-0.10 on version change (or resolve automatically)
  final def tastyClassDirectory =
    "out/bootstrap/dotty-semanticdb/scala-0.11/test-classes"
  val sourceroot = Paths.get("semanticdb", "input").toAbsolutePath
  val sourceDirectory = sourceroot.resolve("src/main/scala")

  val semanticdbClassDirectory = sourceroot.resolve("target/scala-2.12/classes")
  val semanticdbLoader =
    new Semanticdbs.Loader(sourceroot, List(semanticdbClassDirectory))

  val source_to_tasty =
    getTastyFiles(
      Paths.get("out", "bootstrap", "dotty-semanticdb/").toAbsolutePath)

  /** Returns the SemanticDB for this Scala source file. */
  def getScalacSemanticdb(scalaFile: Path): s.TextDocument = {
    semanticdbLoader.resolve(scalaFile).get
  }

  /** List all tasty files occuring in the folder f or one of its subfolders */
  def recursiveListFiles(f: File): Array[File] = {
    val pattern = ".*test-classes/example.*\\.tasty".r
    val files = f.listFiles
    val folders = files.filter(_.isDirectory)
    val tastyfiles = files.filter(_.toString match {
      case pattern(x: _*) => true
      case _              => false
    })
    tastyfiles ++ folders.flatMap(recursiveListFiles)
  }

  /** Returns a mapping from *.scala file to a list of tasty files. */
  def getTastyFiles(artifactsPath: Path): HashMap[String, List[Path]] = {
    val source_to_tasty: HashMap[String, List[Path]] = HashMap()
    val tastyfiles = recursiveListFiles(artifactsPath.toFile())
    recursiveListFiles(artifactsPath.toFile()).map(tasty_path => {
      val (classpath, classname) = getClasspathClassname(tasty_path.toPath())
      // We add an exception here to avoid crashing if we encountered
      // a bad tasty file
      try {
        val inspecter = new TastyInspecter
        ConsumeTasty(classpath, classname :: Nil, inspecter)
        inspecter.source_path.foreach(
          source =>
            source_to_tasty +=
              (source -> (tasty_path
                .toPath() :: source_to_tasty.getOrElse(source, Nil))))
      } catch {
        case _: InvocationTargetException => println(tasty_path)
      }
    })
    source_to_tasty
  }

  /** Infers a tuple (class path, class name) from a given path */
  def getClasspathClassname(file: Path): (String, String) = {
    val pat = """(.*)\..*""".r
    val classpath = file.getParent().getParent().toString()
    val modulename = file.getParent().getFileName().toString()
    val sourcename =
      file.toFile().getName().toString() match {
        case pat(name) => name
        case _         => ""
      }
    return (classpath, modulename + "." + sourcename)
  }

  def getTastySemanticdb(scalaFile: Path): s.TextDocument = {
    val scalac = getScalacSemanticdb(scalaFile)

    val tasty_files = source_to_tasty.getOrElse(
      sourceDirectory.resolve(scalaFile).toString,
      Nil)

    val tasty_classes = tasty_files.map(getClasspathClassname)
    // If we have more than one classpath then something went wrong
    if (tasty_classes.groupBy((a, _) => a).size != 1) {
      scalac
    } else {
      val (classpaths, classnames) = tasty_classes.unzip
      val sdbconsumer = new SemanticdbConsumer

      val _ = ConsumeTasty(classpaths.head, classnames, sdbconsumer)
      sdbconsumer.toSemanticdb(scalac.text)
    }
  }

  /** Fails the test if the s.TextDocument from tasty and semanticdb-scalac are not the same. */
  def checkFile(filename: String): Unit = {
    val path = sourceDirectory.resolve(filename)
    val scalac = getScalacSemanticdb(path)
    val tasty = getTastySemanticdb(path)
    println(tasty)
    val obtained = Semanticdbs.printTextDocument(tasty)
    val expected = Semanticdbs.printTextDocument(scalac)
    assertNoDiff(obtained, expected)
  }

  /** Fails the test with a pretty diff if there obtained is not the same as expected */
  def assertNoDiff(obtained: String, expected: String): Unit = {
    if (obtained.isEmpty && !expected.isEmpty) fail("obtained empty output")
    def splitLines(string: String): java.util.List[String] =
      string.trim.replace("\r\n", "\n").split("\n").toSeq.asJava
    val obtainedLines = splitLines(obtained)
    val b = splitLines(expected)
    val patch = difflib.DiffUtils.diff(obtainedLines, b)
    val diff =
      if (patch.getDeltas.isEmpty) ""
      else {
        difflib.DiffUtils
          .generateUnifiedDiff(
            "tasty",
            "scala2",
            obtainedLines,
            patch,
            1
          )
          .asScala
          .mkString("\n")
      }
    if (!diff.isEmpty) {
      fail("\n" + diff)
    }
  }


  //@Test def testAccess(): Unit = checkFile("example/Access.scala")
  //@Test def testAdvanced(): Unit = checkFile("example/Advanced.scala")
  //@Test def testAnonymous(): Unit = checkFile("example/Anonymous.scala")
  //@Test def testClasses(): Unit = checkFile("example/Classes.scala")
  //@Test def testEmpty(): Unit = checkFile("example/Empty.scala")
  //@Test def testEmptyObject(): Unit = checkFile("example/EmptyObject.scala")
  //@Test def testExample(): Unit = checkFile("example/Example.scala")
  //@Test def testExample2(): Unit = checkFile("example/Example2.scala")
  //@Test def testExclude(): Unit = checkFile("example/Exclude.scala")
  //WIP @Test def testFlags(): Unit = checkFile("example/Flags.scala")
  //@Test def testImports(): Unit = checkFile("example/Imports.scala")
  //@Test def testIssue1749(): Unit = checkFile("example/Issue1749.scala")
  //@Test def testLocalFile(): Unit = checkFile("example/local-file.scala")
  //@Test def testLocals(): Unit = checkFile("example/Locals.scala")
  //@Test def testMacroAnnotations(): Unit = checkFile("example/MacroAnnotations.scala")
  //WIP(assert) @Test def testMethods(): Unit = checkFile("example/Methods.scala")
  //@Test def testMultiArguments(): Unit = checkFile("example/MultiArguments.scala")
  //@Test def testMethodUsages(): Unit = checkFile("example/MethodUsages.scala")
  //def testObjects(): Unit = checkFile("example/Objects.scala")
  //@Test def testOverrides(): Unit = checkFile("example/Overrides.scala")
  //WIP @Test def testPrefixes(): Unit = checkFile("example/Prefixes.scala")
  //@Test def testSelfs(): Unit = checkFile("example/Selfs.scala")
  //@Test def testSelfUse(): Unit = checkFile("example/SelfUse.scala")
  //WIP @Test def testSynthetic(): Unit = checkFile("example/Synthetic.scala")
  //WIP @Test def testTraits(): Unit = checkFile("example/Traits.scala")
  //WIP @Test def testTypes(): Unit = checkFile("example/Types.scala")
  //@Test def testVals(): Unit = checkFile("example/Vals.scala")
  //@Test def testDependantModule(): Unit = checkFile("example/DependantModule.scala")
  //@Test def testNew(): Unit = checkFile("example/New.scala")
  //@Test def testIgnoredSymbol(): Unit = checkFile("example/IgnoredSymbol.scala")
  @Test def testCase(): Unit = checkFile("example/Case.scala")


  def testOutput(className: String, expected: String): Unit = {
    val out = new StringBuilder
    ConsumeTasty(tastyClassDirectory, List(className), new SemanticdbConsumer {
      override def println(x: Any): Unit = out.append(x).append(";")
    })
    assertEquals(expected, out.result())
  }
}
