trait Base[D <: Domain] {
  def f(pp: Extract[D]): Unit
}

class Ext extends Base[ExtDomain] {
  def f(pp: String) = println(pp.length)
}

type Domain
type DomainImpl[T] <: Domain
type ExtDomain = DomainImpl[String]

type Extract[X] = X match { case DomainImpl[t] => t }

object Test {
  def main(args: Array[String]): Unit = {
    def f[M <: Domain](rc: Base[M], v: Extract[M]): Unit =  rc.f(v)
    f(new Ext, "foo")
  }
}
