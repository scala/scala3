sealed trait Ty {
  type T
}

class TUnit() extends Ty {
  type T = Unit
}

case object TUnit extends TUnit()

final case class TFun(dom: Ty, cod: Ty) extends Ty {
  type T = dom.T => cod.T
}

def default(ty: Ty): ty.T = (ty: ty.type & Ty) match {
  case a: (ty.type & TUnit) => (): a.T
  case a: (ty.type & TFun) =>
    val f = { (x: a.dom.T) => default(a.cod) }
    f: a.T
}
