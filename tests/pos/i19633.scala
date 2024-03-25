object Repro {
  trait Responsive {
    type Response
  }

  object Responsive {
    type Aux[R] = Responsive {type Response = R}
    type Response[R] = R match {case Aux[r] => r}
  }

  case class StringRequest(name: String) extends Responsive {
    type Response = String
  }

  def withImplicit[R <: Responsive](request: R)(implicit ct: scala.reflect.ClassTag[Responsive.Response[R]]): Responsive.Response[R] = ???

  def withFunction[R <: Responsive](request: R)(call: R => Responsive.Response[R]): Responsive.Response[R] = ???

  def stringWithFunction(req: StringRequest): String = withFunction(req)(_.getClass.getSimpleName)

  def stringWithImplicit(req: StringRequest): String = withImplicit(req)

  def main(args: Array[String]): Unit = {}
}
