extension (x: Int)
  def add(y: Int): Int = x + y

def newFunction: Int => Int = 4.add

@main def Test = assert(newFunction(1) == 5)