package dotty.tools.pc.utils

import scala.collection.mutable.ListBuffer

import scala.meta.internal.jdk.CollectionConverters._
import dotty.tools.pc.utils.InteractiveEnrichments.*

import org.eclipse.lsp4j.InlayHint
import org.eclipse.lsp4j.TextEdit
import org.eclipse.{lsp4j => l}

object TestInlayHints {

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
  def decorationString(inlayHint: InlayHint): String = {
    val buffer = ListBuffer.empty[String]

    val labels = inlayHint.getLabel().nn.asScala match {
      case Left(label) => List(label)
      case Right(labelParts) => labelParts.asScala.map(_.getValue()).toList
    }
    val data =
      inlayHint.getData().asInstanceOf[Array[Any]]
    buffer += "/*"
    labels.zip(data).foreach { case (label, data) =>
      buffer += label.nn
      buffer ++= readData(data)
    }
    buffer += "*/"
    buffer.toList.mkString
  }

  private def readData(data: Any): List[String] = {
    data match {
      case data: String if data.isEmpty => Nil
      case data: String => List("<<", data, ">>")
      case data: l.Position =>
        val str = s"(${data.getLine()}:${data.getCharacter()})"
        List("<<", str, ">>")
    }
  }

  def applyInlayHints(text: String, inlayHints: List[InlayHint]): String = {
    val textEdits = inlayHints.map { hint =>
      val newText = decorationString(hint)
      val range = new l.Range(hint.getPosition(), hint.getPosition())
      new TextEdit(
        range,
        newText
      )
    }
    TextEdits.applyEdits(text, textEdits)
  }

  def removeInlayHints(text: String): String =
    text.replaceAll(raw"\/\*(.*?)\*\/", "").nn

}
