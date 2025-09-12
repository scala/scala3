import language.experimental.captureChecking
import caps.{cap, use}

def foo1(@use xs: List[() => Unit]): Unit =
  var x: () ->{xs*} Unit = xs.head
  var ys = xs
  while ys.nonEmpty do
    ys = ys.tail
    x = ys.head

def foo2(@use xs: List[() => Unit]): Unit =
  def inner[@use C^](xs: List[() ->{C} Unit]): Unit =
    var x: () ->{C} Unit = xs.head
    var ys = xs
    while ys.nonEmpty do
      ys = ys.tail
      x = ys.head
  inner(xs)

def foo3[@use C^](xs: List[() ->{C} Unit]): Unit =
  var x: () ->{C} Unit = xs.head
  var ys = xs
  while ys.nonEmpty do
    ys = ys.tail
    x = ys.head
