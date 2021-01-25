package dotty.dokka
package tasty.comments

import scala.jdk.CollectionConverters._

import org.jetbrains.dokka.model.{doc => dkkd}
import com.vladsch.flexmark.util.{ast => mdu}

object kt:
  import kotlin.collections.builders.{ListBuilder => KtListBuilder, MapBuilder => KtMapBuilder}

  def emptyList[T] = new KtListBuilder[T]().build()
  def emptyMap[A, B] = new KtMapBuilder[A, B]().build()

object dkk:
  def p(children: dkkd.DocTag*) =
    dkkd.P(children.asJava, Map.empty.asJava)
  def p(params: (String, String)*)(children: dkkd.DocTag*) =
    dkkd.P(children.asJava, params.toMap.asJava)

  def text(str: String) = dkkd.Text(str, JList(), Map.empty.asJava)

  def a(children: dkkd.DocTag*) =
    dkkd.A(children.asJava, Map.empty.asJava)
  def a(params: (String, String)*)(children: dkkd.DocTag*) =
    dkkd.A(children.asJava, params.toMap.asJava)

  def pre(params: (String, String)*)(children: dkkd.DocTag*) =
    dkkd.Pre(children.asJava, params.toMap.asJava)

  def codeInline(children: dkkd.DocTag*) =
    dkkd.CodeInline(children.asJava, Map.empty.asJava)
  def codeInline(params: (String, String)*)(children: dkkd.DocTag*) =
    dkkd.CodeInline(children.asJava, params.toMap.asJava)
  def codeBlock(children: dkkd.DocTag*) =
    dkkd.CodeBlock(children.asJava, Map.empty.asJava)
  def codeBlock(params: (String, String)*)(children: dkkd.DocTag*) =
    dkkd.CodeBlock(children.asJava, params.toMap.asJava)

  def ul(children: dkkd.DocTag*) =
    dkkd.Ul(children.asJava, Map.empty.asJava)
  def ul(params: (String, String)*)(children: dkkd.DocTag*) =
    dkkd.Ul(children.asJava, params.toMap.asJava)
  def ol(children: dkkd.DocTag*) =
    dkkd.Ol(children.asJava, Map.empty.asJava)
  def ol(params: (String, String)*)(children: dkkd.DocTag*) =
    dkkd.Ol(children.asJava, params.toMap.asJava)
  def li(children: dkkd.DocTag*) =
    dkkd.Li(children.asJava, Map.empty.asJava)
  def li(params: (String, String)*)(children: dkkd.DocTag*) =
    dkkd.Li(children.asJava, params.toMap.asJava)

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
