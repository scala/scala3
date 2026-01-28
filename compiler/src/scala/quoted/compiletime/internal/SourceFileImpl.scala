package scala.quoted.compiletime.internal

import dotty.tools.dotc
import dotty.tools.dotc.core.Contexts.*
import scala.quoted.compiletime as pub

type SourceFile = SourceFileImpl
final class SourceFileImpl(val sourceFile: dotc.util.SourceFile) extends pub.SourceFile {

  override lazy val getJPath: Option[java.nio.file.Path] = Option(sourceFile.file.jpath)

  override lazy val name: String = sourceFile.name

  override lazy val path: String = sourceFile.path

  override lazy val content: Option[String] =
    // TODO detect when we do not have a source and return None
    Some(new String(sourceFile.content()))

  override def toString: String = sourceFile.toString
  override lazy val hashCode: Int = sourceFile.hashCode
  override def equals(that: Any): Boolean = that.asInstanceOf[Matchable] match
    case that: SourceFileImpl => this.sourceFile == that.sourceFile
    case _                    => false

}
object SourceFileImpl {

  final class Module(using val ctx: Context) extends pub.SourceFile.Module {

    override def current: SourceFile =
      ??? // FIX-PRE-MERGE (KR) :

  }

}
