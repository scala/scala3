class MetricsGroup[A]
object MetricsGroup:
  import scala.quoted.*

  transparent inline final def refine[A]: MetricsGroup[A] =
    ${ refineImpl[A] }

  private def refineImpl[A](using qctx: Quotes, tpe: Type[A]): Expr[MetricsGroup[A]] =
    import qctx.reflect.*

    val mt = MethodType(Nil)(_ => Nil, _ => TypeRepr.of[A])
    val tpe = Refinement(TypeRepr.of[MetricsGroup[A]], "apply", mt).asType
    tpe match
      case '[tpe] =>
        '{ MetricsGroup[A]().asInstanceOf[MetricsGroup[A] & tpe] }
