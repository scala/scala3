import scala.language.implicitConversions

sealed abstract class CtorType[P]

object CtorType {
  final class Props[P] extends CtorType[P] {
    def whyHelloThere(props: P): Unit = ()
  }
}

trait Comp[P, CT[p] <: CtorType[p]] {
  val ctor: CT[P]
}
object Comp {
  implicit def autoCtor[P, CT[p] <: CtorType[p]](c: Comp[P, CT]): CT[P] = c.ctor
}

object Test {
  val comp: Comp[Int, CtorType.Props] = ???

  comp.whyHelloThere(3)

  Comp.autoCtor(comp).whyHelloThere(3)
}
