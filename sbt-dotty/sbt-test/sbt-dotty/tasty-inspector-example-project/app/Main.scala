package hello

import scala.quoted._
import scala.tasty.inspector._

import scala.jdk.StreamConverters._

import java.nio.file.{Path, Files, Paths, FileSystems}

object Main extends App {

  val inspector = new Inspector {
    def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit = {
      for tasty <- tastys do
        val tastyStr = tasty.ast.show
        println(tastyStr)

    }
  }

  private def walk(f: Path): LazyList[Path] = Files.walk(f).toScala(LazyList)

  locally {

    val `lib/Foo.tasty` = FileSystems.getDefault.getPathMatcher("glob:**/lib/Foo.tasty")
    val pwd = Paths.get("")

    val tastyFiles = for p <- walk(pwd) if `lib/Foo.tasty`.matches(p) yield p.toString

    TastyInspector.inspectTastyFiles(List(tastyFiles.head))(inspector)

  }

}
