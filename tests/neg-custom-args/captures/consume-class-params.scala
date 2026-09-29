class C

class A1(val x: C^)

class A2(x0: C^):
  val x = x0

class A3(x0: C^):
  def x = x0

class A4(x0: C^)

class B1(consume val x: C^)

class B2(consume x0: C^):
  val x = x0

class B3(consume x0: C^):
  def x = x0

class B4(consume x0: C^)

def test(c: C^) =
  val a1 = A1(c)
  val _: A1 = a1  // error
  val a2 = A2(c)
  val _: A2 = a2  // error
  val a3 = A3(c)
  val _: A3 = a3  // error
  val a4 = A4(c)
  val _: A4 = a4  // error
  val b1 = B1(c)
  val _: B1 = b1  // error
  val b2 = B2(c)
  val _: B2 = b2  // error
  val b3 = B3(c)
  val _: B3 = b3  // error
  val b4 = B4(c)
  val _: B4 = b4  // error

class IO

def test2(io: IO^) =

  class A1(val x: C^{io})

  class A2(x0: C^{io}):
    val x = x0

  class A3(x0: C^{io}):
    def x = x0

  class A4(x0: C^{io})

  class B1(consume val x: C^{io})

  class B2(consume x0: C^{io}):
    val x: C^{io} = x0

  class B3(consume x0: C^{io}):
    def x: C^{io} = x0

  class B4(consume x0: C^{io})

  def test(c: C^{io}) =
    val a1 = A1(c)
    val _: A1 = a1  // error
    val a2 = A2(c)
    val _: A2 = a2  // error
    val a3 = A3(c)
    val _: A3 = a3  // error
    val a4 = A4(c)
    val _: A4 = a4  // error
    val b1 = B1(c)
    val _: B1 = b1  // error
    val b2 = B2(c)
    val _: B2 = b2  // error
    val b3 = B3(c)
    val _: B3 = b3  // error
    val b4 = B4(c)
    val _: B4 = b4  // error

