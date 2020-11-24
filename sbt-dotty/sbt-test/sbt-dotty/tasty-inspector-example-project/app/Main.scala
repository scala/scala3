package hello

import scala.quoted._
import scala.tasty.inspector.TastyInspector

import scala.jdk.StreamConverters._

import java.nio.file.{Path, Files, Paths, FileSystems}

object Main extends App {

  val inspector = new TastyInspector {
    protected def processCompilationUnit(using Quotes)(root: quotes.reflect.Tree): Unit = {
      val tastyStr = root.show
      println(tastyStr)
    }
  }

  private def walk(f: Path): LazyList[Path] = Files.walk(f).toScala(LazyList)

  locally {

    val `lib/Foo.tasty` = FileSystems.getDefault.getPathMatcher("glob:**/lib/Foo.tasty")
    val pwd = Paths.get("")

    val tastyFiles = for p <- walk(pwd) if `lib/Foo.tasty`.matches(p) yield p.toString

    inspector.inspectTastyFiles(List(tastyFiles.head))

  }

}
