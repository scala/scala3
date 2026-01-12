//> using options -source 3.7
import language.experimental.captureChecking
// no separation checking
import caps.{any, use}

def foo1(@use xs: List[() => Unit]): Unit =
  var x: () ->{xs*} Unit = xs.head
  var ys = xs
  while ys.nonEmpty do
    ys = ys.tail
    x = ys.head

def foo2(@use xs: List[() => Unit]): Unit =
  var x: () => Unit = xs.head // note: this would fail separation checking
  var ys = xs
  while ys.nonEmpty do
    ys = ys.tail
    x = ys.head

def foo3[@use C^](xs: List[() ->{C} Unit]): Unit =
  var x: () ->{C} Unit = xs.head
  var ys = xs
  while ys.nonEmpty do
    ys = ys.tail
    x = ys.head
