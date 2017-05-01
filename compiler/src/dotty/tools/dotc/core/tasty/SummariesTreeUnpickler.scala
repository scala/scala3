package dotty.tools.dotc.core.tasty

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.core.Names.TermName
import dotty.tools.dotc.core.tasty.TastyBuffer.{Addr, NameRef}

import scala.collection.mutable

class SummariesTreeUnpickler(override val symAtAddr: mutable.HashMap[Addr, Symbol], reader: TastyReader, tastyName: NameRef => TermName, sectionName: String)
    extends TreeUnpickler(reader, tastyName, posUnpicklerOpt = None) {

  roots = Set.empty

  def getStartReader(implicit ctx: Context): Option[TreeReader] = {
    val st = new TreeReader(reader)
    st.skipToplevel()(ctx.addMode(Mode.AllowDependentFunctions))

    while (true) {
      while (reader.nextByte != TastyFormat.VALDEF && !reader.isAtEnd) st.skipTree()
      if (reader.isAtEnd) return None // no section here
      val tag = reader.readByte()
      val end = reader.readEnd()
      val name = st.readName()
      if (name.toString == sectionName) return Some(st.forkAt(end))
      st.skipTree() // skip type
      st.skipTree() // skip rhs
    }

    None
  }

}
