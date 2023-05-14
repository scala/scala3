import java.lang.ProcessHandle // error: not a member

object Jdk9App extends App {
  println(ProcessHandle.current().pid()) // error: not found
}
