package dotty.tools.debug

import com.sun.jdi.*
import scala.jdk.CollectionConverters.*
import scala.concurrent.duration.Duration

class Debugger(vm: VirtualMachine, maxDuration: Duration):
  export vm.dispose

object Debugger:
  // The socket JDI connector
  private val connector = Bootstrap.virtualMachineManager
    .attachingConnectors
    .asScala
    .find(_.getClass.getName == "com.sun.tools.jdi.SocketAttachingConnector")
    .get

  def apply(jdiPort: Int, maxDuration: Duration): Debugger =
    val arguments = connector.defaultArguments()
    arguments.get("hostname").setValue("localhost")
    arguments.get("port").setValue(jdiPort.toString)
    arguments.get("timeout").setValue(maxDuration.toMillis.toString)
    val vm = connector.attach(arguments)
    new Debugger(vm, maxDuration)

