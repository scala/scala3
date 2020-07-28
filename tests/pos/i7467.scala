import javax.swing._
import java.awt._

class DuplicateSymbolError_DirectSuperclass extends DefaultListCellRenderer() {
  override def getListCellRendererComponent(list: JList[_ <: Object], value: Object, index: Int, isSelected: Boolean, cellHasFocus: Boolean): Component = ???
}

class DuplicateSymbolError_IndirectInterface extends DefaultListCellRenderer() {
  override def getListCellRendererComponent(list: JList[_], value: Object, index: Int, isSelected: Boolean, cellHasFocus: Boolean): Component = ???
}
