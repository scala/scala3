import language.experimental.captureChecking
// no separation checking
import caps.consume

def localReach() =
  val xs: List[() => Unit] = ???
  var ys: List[() ->{xs*} Unit] = xs
  var x: () ->{xs*} Unit = ys.head
  while ys.nonEmpty do
    ys = ys.tail
    x = ys.head

def localReach2(op: () => Unit) =
  val xs: List[() => Unit] = op :: Nil // error
  var ys: List[() ->{xs*} Unit] = xs
  var x: () ->{xs*} Unit = ys.head
  while ys.nonEmpty do
    ys = ys.tail
    x = ys.head

def localReach3(ops: List[() => Unit]) =
  val xs: List[() => Unit] = ops // error
  var ys: List[() ->{xs*} Unit] = xs
  var x: () ->{xs*} Unit = ys.head // error
  while ys.nonEmpty do
    ys = ys.tail
    x = ys.head  // error

