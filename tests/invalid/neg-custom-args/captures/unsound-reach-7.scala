import language.experimental.captureChecking
import caps.{any, use}

trait IO
trait Async

def main(io: IO^, async: Async^) =
  def bad[X](ops: List[(X, () ->{io} Unit)])(f: () ->{ops*} Unit): () ->{io} Unit = f // error
  def runOps(@use ops: List[(() => Unit, () => Unit)]): () ->{ops*} Unit =
    () => ops.foreach((f1, f2) => { f1(); f2() }) // error (???)
  def delayOps(@use ops: List[(() ->{async} Unit, () ->{io} Unit)]): () ->{io} Unit =
    val runner: () ->{ops*} Unit = runOps(ops)
    val badRunner: () ->{io} Unit = bad[() ->{async} Unit](ops)(runner)
      // it uses both async and io, but we losed track of async.
    badRunner