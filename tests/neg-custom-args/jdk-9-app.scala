import java.lang.ProcessHandle

object Jdk9App extends App {
  println(ProcessHandle.current().pid()) // error: not found
}
