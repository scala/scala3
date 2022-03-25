package dotty.tools.scaladoc
package tasty
package comments

import org.junit.{Test, Rule}
import org.junit.Assert.{assertSame, assertTrue, assertEquals}
import com.vladsch.flexmark.util.{ast => mdu, sequence}
import com.vladsch.flexmark.{ast => mda}

import wiki.WikiDocElement

abstract class MemberGroupingTests(syntax: String) extends ScaladocTest("groups") {
  import MemberGroupingTests._
  override def moduleDocContext = super.moduleDocContext.copy(args = super.moduleDocContext.args.copy(defaultSyntax = List(syntax)))

  val expectedMemberGroups = Map(
    "groupFoo" -> "foo",
    "groupBar" -> "bar",
    "groupBazz" -> "bazz"
  )

  val expectedDescriptions = Map(
    "bar" -> Seq("Description", "of", "group", "bar")
  )

  val expectedNames = Map(
    "foo" -> "Foo-group"
  )

  val expectedPrio = Map(
    "bazz" -> 10
  )

  override def runTest = withModule { module =>
    val memberGroups = module.members.values.flatMap { member =>
      member.docs.flatMap(_.group).map(g => member.name -> g)
    }.toMap

    val groupDescriptions: Map[String, String] = module.members.values
      .flatMap { member =>
        member.docs.map(_.groupDesc)
      }
      .foldLeft[Map[String, DocPart]](Map.empty)(_ ++ _)
      .map {
        case (k, s: Seq[WikiDocElement] @unchecked) => k -> s.map {
          case i: wiki.Inline => flatten(i)
          case b: wiki.Block => flatten(b)
        }.mkString
        case (k, n: mdu.Node) => k -> n.getChars.toString
      }

    val groupNames = module.members.values
      .flatMap { member =>
        member.docs.map(_.groupNames)
      }
      .foldLeft[Map[String, DocPart]](Map.empty)(_ ++ _)
      .map {
        case (k, s: Seq[WikiDocElement] @unchecked) => k -> s.map {
          case i: wiki.Inline => flatten(i)
          case b: wiki.Block => flatten(b)
        }.mkString
        case (k, n: mdu.Node) => k -> n.getChars.toString
      }

    val groupPrios = module.members.values
      .flatMap { member =>
        member.docs.map(_.groupPrio)
      }
      .foldLeft(Map.empty)(_ ++ _)

    expectedMemberGroups.foreach {
      case (memberName, groupName) => assertEquals(memberGroups.getOrElse(memberName, ""), groupName)
    }

    expectedDescriptions.foreach {
      case (memberName, expectedWords) => assert(expectedWords.forall(word => groupDescriptions.getOrElse(memberName, "").contains(word)))
    }

    expectedNames.foreach {
      case (group, groupName) => assertEquals(groupNames.getOrElse(group, ""), groupName)
    }

    expectedPrio.foreach {
      case (group, prio) => assertEquals(groupPrios.getOrElse(group, -1), prio)
    }
  }
}

object MemberGroupingTests {
  private def flatten(b: wiki.Inline): String = b match
    case wiki.Text(t) => t
    case wiki.Italic(t) => flatten(t)
    case wiki.Bold(t) => flatten(t)
    case wiki.Underline(t) => flatten(t)
    case wiki.Superscript(t) => flatten(t)
    case wiki.Subscript(t) => flatten(t)
    case wiki.Link(_, t) => t.fold("")(flatten)
    case wiki.Monospace(t) => flatten(t)
    case wiki.RepresentationLink(t, _) => flatten(t)
    case wiki.Chain(elems) => elems.headOption.fold("")(flatten)
    case wiki.HtmlTag(t) => t
    case wiki.Summary(t) => flatten(t)

  private def flatten(b: wiki.Block): String = b match
    case wiki.Paragraph(text) => flatten(text)
    case wiki.Title(text, _) => flatten(text)
    case wiki.Code(text) => text
    case wiki.UnorderedList(elems) => elems.headOption.fold("")(flatten)
    case wiki.OrderedList(elems, _) => elems.headOption.fold("")(flatten)
    case wiki.DefinitionList(items) => items.headOption.fold("")(e => flatten(e._1))
    case wiki.HorizontalRule => ""
    case wiki.Table(header, columns, rows) => (header +: rows).flatMap(_.cells).flatMap(_.blocks).map(flatten).mkString
}

class WikiMemberGroupingTests extends MemberGroupingTests("wiki")

class MarkdownMemberGroupingTests extends MemberGroupingTests("markdown")
