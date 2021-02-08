import java.awt.event.WindowEvent
import java.awt.Window as AWTWindow

abstract class Window {
  def peer: AWTWindow with InterfaceMixin

  protected trait InterfaceMixin extends javax.swing.RootPaneContainer

  protected trait SuperMixin extends java.awt.Window {
    override protected def processWindowEvent(e: WindowEvent): Unit = {
      super.processWindowEvent(e)
      if (e.getID == WindowEvent.WINDOW_CLOSING)
        closeOperation()
    }
  }

  def closeOperation(): Unit = ()
}
