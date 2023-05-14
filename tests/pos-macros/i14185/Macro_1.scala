import scala.quoted.*

object Test {
  inline def foo[A, M[_]]: Unit = ${ fooImpl[A, M] }

  private def fooImpl[A, M[_]](
      using
      q: Quotes,
      tc: Type[M],
      pt: Type[A]
    ): Expr[Unit] = {
    import q.reflect.*

    val ptpe = TypeRepr.of[A]
    val neededGivenType = TypeRepr.of[M](using tc).appliedTo(ptpe)

    val neededGiven: Option[Term] = Implicits.search(neededGivenType) match {
      case suc: ImplicitSearchSuccess =>
        Some(suc.tree)

      case _ =>
        None
    }

    neededGiven.map(_.show)

    '{ () }
  }
}

// ---

/** Type level evidence that type `A` is not type `B`. */
final class IsNot[A, B]() {
  override val toString = "not"
}

object IsNot {
  implicit def defaultEvidence[A, B]: IsNot[A, B] = new IsNot[A, B]()

  @annotation.implicitAmbiguous("Could not prove type ${A} is not (IsNot) ${A}")
  implicit def ambiguousEvidence1[A]: IsNot[A, A] = null
  implicit def ambiguousEvidence2[A]: IsNot[A, A] = null
}

// ---

sealed trait SomeTypeclass[T]

object SomeTypeclass extends SomeTypeclassLowPrio {

  given collection[T, Repr <: Iterable[T]](
      using SomeTypeclass[T],
      Repr IsNot Option[T]
  ): SomeTypeclass[Repr] = new SomeTypeclass[Repr] {}
}

sealed trait SomeTypeclassLowPrio {
  given int: SomeTypeclass[Int] = new SomeTypeclass[Int] {}
}
