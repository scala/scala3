import language.experimental.captureChecking
import caps.{any, use}

def foo[C^](xs: List[() ->{C} Unit]): Unit =
  var x: () ->{C} Unit = xs.head
  var ys = xs
  while ys.nonEmpty do
    ys = ys.tail
    x = ys.head
