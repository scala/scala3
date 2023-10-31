import javax.swing.*
import java.awt.*

class DuplicateSymbolError_DirectSuperclass extends DefaultListCellRenderer() {
  override def getListCellRendererComponent(list: JList[? <: Object], value: Object, index: Int, isSelected: Boolean, cellHasFocus: Boolean): Component = ???
}

class DuplicateSymbolError_IndirectInterface extends DefaultListCellRenderer() {
  override def getListCellRendererComponent(list: JList[?], value: Object, index: Int, isSelected: Boolean, cellHasFocus: Boolean): Component = ???
}
