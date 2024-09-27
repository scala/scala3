import language.experimental.captureChecking

trait File
val useFile: [R] -> (path: String) -> (op: File^ -> R) -> R = ???
def main(): Unit =
  val f: [R] -> (path: String) -> (op: File^ -> R) -> R = useFile
  val g: [R] -> (path: String) -> (op: File^{f*} -> R) -> R = f  // error
  val leaked = g[File^{f*}]("test")(f => f)  // error
