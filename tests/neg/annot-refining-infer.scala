class MyAnnotation(x: Any) extends scala.annotation.RefiningAnnotation

def id[T](x: T): T = x
def id2[T](x: T, y: T): T = x

def foo1[T](x: T, g: T => Unit): T = x
def foo2[T](x: T, y: T, g: T => Unit): T = x
def foo3[T](g: T => Unit, x: T, y: T): T = x
def foo4[T](x: T, g: T => Unit, h: T => Unit): T = x

def take42[T](x: T @MyAnnotation(42)): Unit = ()
def take43[T](x: T @MyAnnotation(43)): Unit = ()
def take42or43[S](x: S @MyAnnotation(42) | S @MyAnnotation(43)): Unit = ()
def take42or43Int(x: Int @MyAnnotation(42) | Int @MyAnnotation(43)): Unit = ()

def main =
  val c42: Int @MyAnnotation(42) = ???
  val c43: Int @MyAnnotation(43) = ???

  val v01 = id2[Int @MyAnnotation(42) | Int @MyAnnotation(43)](c42, c43)
  val v02: Int @MyAnnotation(42) | Int @MyAnnotation(43) = c42
  val v03: Int @MyAnnotation(42) | Int @MyAnnotation(43) = id2(c42, c43)

  val v04 = foo1(c42, take42)
  val v05: Int @MyAnnotation(42) = v13
  val v06 = foo1(c42, take43) // error
  val v07 = foo1(c42, take42or43)

  val v08 = foo2(c42, c42, take42)
  val v09: Int @MyAnnotation(42) = v15
  val v10 = foo2(c42, c43, take42) // error
  val v11 = foo2(c42, c43, take42or43) // error
  val v12 = foo2(c42, c43, take42or43Int)

  val v13 = foo3(take42or43, c42, c43) // error
  val v14 = foo3(take42or43Int, c42, c43)

  val v15 = foo4(c42, take42, take42)
  val v16: Int @MyAnnotation(42) = v15
  val v17 = foo4(c42, take42, take43) // error
