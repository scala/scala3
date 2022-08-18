class Box[T](x: T)

class Cov[+T](x: T)
class Contrav[-T](x: T)

type BoxElem[X] = X match
  case Box[a] => a

type BoxToList[X] = X match
  case Box[a] => List[a]

type CovElem[X] = X match
  case Cov[a] => a

type CovToList[X] = X match
  case Cov[a] => List[a]

type ContravElem[X] = X match
  case Contrav[a] => a

type ContravToList[X] = X match
  case Contrav[a] => List[a]

class C

def f[X <: Box[C], Y <: Cov[C], Z <: Contrav[C]] =
  def a: BoxElem[X] = ???  // OK
  val _: C = a

  def a1: CovElem[Y] = ???
  val _: C = a1  // error

  def a2: ContravElem[Z] = ???
  val _: C = a2  // error

  def b: BoxToList[X] = ???  // OK
  val _: List[C] = b

  def b1: CovToList[Y] = ???
  val _: List[C] = b1  // error

  def b2: ContravElem[Z] = ???
  val _: List[C] = b2  // error



