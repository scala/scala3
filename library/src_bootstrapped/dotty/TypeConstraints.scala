package dotty

import scala.annotation.implicitNotFound

object TypeConstraints {

  /**
   * An instance of `A <::< B` witnesses that `A` is a subtype of `B`.
   * Requiring an implicit argument of the type `A <::< B` encodes
   * the generalized constraint `A <: B`.
   *
   * To constrain any abstract type T that's in scope in a method's
   * argument list (not just the method's own type parameters) simply
   * add an implicit argument of type `T <::< U`, where `U` is the required
   * upper bound; or for lower-bounds, use: `L <::< T`, where `L` is the
   * required lower bound.
   *
   * In part contributed by Jason Zaugg.
   * Addapted to phantom types by Nicolas Stucki.
   */
  @implicitNotFound(msg = "Cannot prove that ${From} <::< ${To}.")
  type <::<[-From, +To] <: PhantomConstraints.Evidence

  /** An instance of `A =::= B` witnesses that the types `A` and `B` are equal.
   *
   * @see `<::<` for expressing subtyping constraints
   */
  @implicitNotFound(msg = "Cannot prove that ${From} =::= ${To}.")
  type =::=[From, To] <: (From <::< To)

  implicit inline def $tpEquals[X]: X =::= X = PhantomConstraints.evidence

  implicit inline def cast[From, To](x: From)(implicit ev: From <::< To): To = x.asInstanceOf[To]
  implicit inline def invCast[From, To](x: To)(implicit ev: From =::= To): From = x.asInstanceOf[From]

  @deprecated("Use TypeConstraints.cast implicitly to avoid runtime overhead", "dotty")
  implicit class Cast[From, To](ev: From <::< To) extends (From => To) with Serializable {
    inline def apply(x: From): To = x.asInstanceOf[To] // Safe cast ensured by evidence `ev`
  }

  private[TypeConstraints] object PhantomConstraints extends Phantom {
    type Evidence <: this.Any
    inline def evidence = assume
  }

}
