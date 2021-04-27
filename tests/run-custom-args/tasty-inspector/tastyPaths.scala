import scala.quoted.*
import scala.tasty.inspector.*

import java.io.File.separatorChar

opaque type PhoneNumber = String

case class I8163() {
  val phone: PhoneNumber = "555-555-5555".asInstanceOf[PhoneNumber]
  val other: String = "not a phone"
}

object Test {
  def main(args: Array[String]): Unit = {
    // Artefact of the current test infrastructure
    // TODO improve infrastructure to avoid needing this code on each test
    val classpath = dotty.tools.dotc.util.ClasspathFromClassloader(this.getClass.getClassLoader).split(java.io.File.pathSeparator).find(_.contains("runWithCompiler")).get
    val allTastyFiles = dotty.tools.io.Path(classpath).walkFilter(_.extension == "tasty").map(_.toString).toList
    val tastyFiles = allTastyFiles.filter(_.contains("I8163"))

    TastyInspector.inspectTastyFiles(tastyFiles)(new TestInspector())
  }
}

class TestInspector() extends Inspector:

  def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
    println(tastys.map(_.path.split("tasty-inspector").last.replace(separatorChar, '/')))
    try
      quotes.reflect.SourceFile.current
      assert(false)
    catch case ex: java.lang.UnsupportedOperationException =>
      println(ex.getMessage) // ok