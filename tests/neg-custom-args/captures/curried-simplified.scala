@annotation.capability class Cap

type Protect[T] = T

def test(x: Cap, y: Cap) =
  def x1: {x} () -> () -> Int = ???
  def y1: () -> () -> Int = x1  // error
  def x2: {x} () -> () => Int = ???
  def y2: () -> () => Int = x2 // error
  def x3: Cap -> Int -> Int = ???
  def y3: Cap -> Protect[Int -> Int] = x3 // error
  def x4: Cap -> Protect[Int -> Int] = ???
  def y4: Cap -> {} Int -> Int = x4 // ok
  def x5: Cap -> {x} Int -> Int = ???
  def y5: Cap -> {} Int -> Int = x5 // error
  def x6: Cap -> Cap -> Int -> Int = ???
  def y6: Cap -> {} Cap -> Protect[Int -> Int] = x6 // error
  def x7: Cap -> (x: Cap) -> Int -> Int = ???
  def y7: Cap -> Protect[Cap -> {} Int -> Int] = x7 // error


