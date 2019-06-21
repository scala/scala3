class Hi
class Lo extends Hi
class Cov1[+T]
class Cov2[+T >: Lo <: Hi]
class Contra1[-T]
class Contra2[-T >: Lo <: Hi]

object Test {
  val a: List[Cov1[Any]] = ??? : List[Cov1[_]]
  val b: List[Cov2[Hi]] = ??? : List[Cov2[_]]
  val c: List[Contra1[Nothing]] = ??? : List[Contra1[_]]
  val d: List[Contra2[Lo]] = ??? : List[Contra2[_]]
}
