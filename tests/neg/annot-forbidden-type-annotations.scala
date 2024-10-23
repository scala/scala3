class annot[T](arg: T) extends scala.annotation.Annotation

def m1(x: Any): Unit = ()
val x: Int = ???

def m2(): String @annot(m1(2)) = ??? // error
def m3(): String @annot(throw new Error()) = ??? // error
def m4(): String @annot((x: Int) => x) = ??? // error
def m5(): String @annot(x + 1) = ??? // error

def main =
  @annot(m1(2)) val x1: String = ??? // ok
  @annot(throw new Error()) val x2: String = ??? // ok
  @annot((x: Int) => x) val x3: String = ??? // ok
  @annot(x + 1) val x4: String = ??? // ok
  ()
