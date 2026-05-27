package dotty.tools.pc.utils

import scala.collection.mutable.ListBuffer
import scala.meta.internal.jdk.CollectionConverters.*
import scala.meta.internal.pc.InlayHints

import dotty.tools.pc.utils.InteractiveEnrichments.*

import com.google.gson.JsonElement
import org.eclipse.lsp4j.InlayHint
import org.eclipse.lsp4j.TextEdit
import org.eclipse.lsp4j as l

object TestInlayHints:

  // For not local symbols - semanticdb symbol
  //  val x = 123
  //      |
  //      v
  //  val x<<: Int/*scala/predef/Int#*/>> = 123
  // For local symbols - definition position in source
  // type T = Int
  // val x: T = ???
  // val y = x
  //     |
  //     v
  // val y<<: T/*(0:5,0:5)*/>> = x
  def decorationString(inlayHint: InlayHint): String =
    val buffer = ListBuffer.empty[String]

    val labels = inlayHint.getLabel().nn.asScala match
      case Left(label) => List(label)
      case Right(labelParts) => labelParts.asScala.map(_.getValue()).toList
    val data =
      InlayHints.fromData(inlayHint.getData().asInstanceOf[JsonElement])._2
    buffer += "/*"
    labels.zip(data).foreach { case (label, data) =>
      buffer += label
      buffer ++= readData(data)
    }
    buffer += "*/"
    buffer.toList.mkString

  private def readData(data: Either[String, l.Position]): List[String] =
    data match
      case Left("") => Nil
      case Left(data) => List("<<", data, ">>")
      case Right(data) =>
        val str = s"(${data.getLine()}:${data.getCharacter()})"
        List("<<", str, ">>")

  def applyInlayHints(text: String, inlayHints: List[InlayHint]): String =
    val textEdits = inlayHints.map { hint =>
      val newText = decorationString(hint)
      val range = new l.Range(hint.getPosition(), hint.getPosition())
      new TextEdit(
        range,
        newText
      )
    }
    TextEdits.applyEdits(text, textEdits)

  def removeInlayHints(text: String): String =
    text.replaceAll(raw"\/\*(.*?)\*\/", "").nn
