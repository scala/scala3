package dotty.tools.languageserver.util.actions

import dotty.tools.languageserver.util.embedded.CodeMarker
import dotty.tools.languageserver.util.{CodeRange, PositionContext}

import scala.collection.JavaConverters._

class CodeReferences(val range: CodeRange, refs: List[CodeRange], withDecl: Boolean) extends ActionOnRange {

  override def onMarker(marker: CodeMarker): Exec[Unit] = {
    val res = server.references(fix(marker.toReferenceParams(withDecl))).get()
    assert(res.size() == refs.size)
    res.asScala.zip(refs).foreach { case (loc, ref) => assert(loc == ref.toLocation, res) }
  }

  override def show: PositionContext.PosCtx[String] =
    s"CodeReferences(${range.show}, ${refs.map(_.show)}, $withDecl)"
}
