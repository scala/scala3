package dotty.tools.dotc

import dotty.tools.vulpix._
import org.junit.Test
import org.junit.Ignore
import java.nio.file.Paths

class Playground:
  import TestConfiguration._
  import CompilationTests._
  

  @Test def example: Unit =
    implicit val testGroup: TestGroup = TestGroup("playground")
    // println(Paths.get(".").toAbsolutePath())
    compileFile("../tests/pos/varargs.scala", defaultOptions).checkCompile()
