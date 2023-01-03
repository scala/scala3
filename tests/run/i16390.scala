inline def cfor(inline body: Int => Unit): Unit =
  var index = 0
  while index < 3 do
    body(index)
    index = index + 1

@main def Test =
  assert(test1() == test2(), (test1(), test2()))

def test1() =
  val b = collection.mutable.ArrayBuffer.empty[() => Int]
  cfor { x =>
    b += (() => x)
  }
  b.map(_.apply()).toList

def test2() =
  val b = collection.mutable.ArrayBuffer.empty[() => Int]
  var index = 0
  while index < 3 do
    ((x: Int) => b += (() => x)).apply(index)
    index = index + 1
  b.map(_.apply()).toList