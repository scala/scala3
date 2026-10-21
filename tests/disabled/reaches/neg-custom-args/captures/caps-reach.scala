import language.experimental.captureChecking
import scala.collection.mutable.ListBuffer

class MyContainer:
  val f: Object^ = ???
  val consumers1 = ListBuffer.empty[() ->{caps.any*} Unit]  // error
  val consumers2 = ListBuffer.empty[() ->{caps.any*.rd} Unit] // error
  val consumers3 = ListBuffer.empty[() ->{f*.rd} Unit] // ok
  val consumers4 = ListBuffer.empty[() ->{f.rd*} Unit] // error
  val consumers5 = ListBuffer.empty[() ->{f.rd.rd} Unit] // error
  val consumers6 = ListBuffer.empty[() ->{f * *} Unit] // error
