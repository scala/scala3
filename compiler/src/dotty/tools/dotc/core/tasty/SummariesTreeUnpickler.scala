package dotty.tools.dotc.core.tasty

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.core.Names.TermName
import dotty.tools.dotc.core.tasty.TastyBuffer.NameRef

class SummariesTreeUnpickler(treeUnpickler: TreeUnpickler, reader: TastyReader, tastyName: NameRef => TermName, sectionName: String)(implicit ctx: Context)
    extends TreeUnpickler(reader, tastyName, posUnpicklerOpt = None) {

  roots = Set.empty

  override val symAtAddr = treeUnpickler.symAtAddr
  override val treeAtAddr = treeUnpickler.treeAtAddr
  override val typeAtAddr = treeUnpickler.typeAtAddr

  def getStartReader: Option[TreeReader] = {
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
