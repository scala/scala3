package dotty.tools.scaladoc
package tasty.comments

import scala.jdk.CollectionConverters._

import com.vladsch.flexmark.util.{ast => mdu}

object dbg:
  case class See(n: mdu.Node, c: Seq[See]) {
    def show(sb: StringBuilder, indent: Int): Unit = {
      sb ++= " " * indent
      sb ++= n.toString
      sb ++= "\n"
      c.foreach { s => s.show(sb, indent + 2) }
    }

    override def toString = {
      val sb = new StringBuilder
      show(sb, 0)
      sb.toString
    }
  }

  def see(n: mdu.Node): See =
    See(n, n.getChildIterator.asScala.map(see).toList)
