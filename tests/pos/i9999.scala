case class A();
case class B();

type M2[X <: A|B] = X match {
  case A => A
  case B => B
}

def f2(x: A|B): M2[x.type] = x match {
  case _: A => A()
  case _: B => B()
}

type M1[X <: A|B] = X match {
  case A => A
  case B => (x: A|B) => M2[x.type]
}

def f1(x: A|B): M1[x.type] = x match {
  case _: A => A()
  case _: B => (x: A|B) => f2(x)
}

case class More(); case class Stop();

sealed abstract class DSL
case class Fun[F <: More|Stop => DSL](cont: F) extends DSL
case class Nop() extends DSL

type Match2[X <: More|Stop] <: DSL = X match {
  case More => Fun[(y: More|Stop) => Match1[y.type]]
  case Stop => Nop
}
type Match1[X] <: DSL = X match {
  case More => Nop
  case Stop => Nop
}

def fun2(x: More|Stop): Match2[x.type] = x match {
  case _: More => Fun(fun1)  // error
  case _: Stop => Nop()
}

def fun1(y: More|Stop): Match1[y.type] = y match {
  case _: More => Nop()
  case _: Stop => Nop()
}
