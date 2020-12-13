package dotty.tools
package dotc
package ast

import org.junit.Test
import org.junit.Assert._

import dotc.core.Contexts._
import dotc.parsing.Parsers.Parser
import dotc.util.SourceFile

class UntpdTreeMapTest extends DottyTest {

  import untpd._

  def parse(code: String): Tree = {
    val (_, stats) = new Parser(SourceFile.virtual("<meta>", code)).templateStatSeq()
    stats match { case List(stat) => stat; case stats => untpd.Thicket(stats) }
  }

  @Test
  def testMapInterpolatedString = {
    val tree = parse(""" q"hello ${2017}!" """)
    val identity = new UntypedTreeMap {
      override def transform(tree: Tree)(using Context): Tree = tree match {
        case _ =>  super.transform(tree)
      }
    }

    assertEquals(tree.toString, identity.transform(tree).toString)
  }
}
