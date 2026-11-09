import language.experimental.captureChecking
import caps.*

class File extends SharedCapability

// Soundness regression test: applying an identity poly-fn must not
// erase the captures of its argument. If the lambda's stored result
// type were scrubbed to `^{}`, an impure value could be claimed pure
// and leaked past a strict capture-set bound.

object Test:
  val id = [C^] => (x: File^{C}) => x

  def check(): Unit =
    val a = File()
    val r: File^{} = id[{a}](a)  // error
