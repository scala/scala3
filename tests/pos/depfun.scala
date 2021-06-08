// The following test is derived from scala/reflect/TypeTest.scala, but using
// a dependent function instead of a dependent SAM. It shows that the special treatment
// using a DependentTypeTree is not needed for plain function types.
// But for SAM types, the treatment is needed, otherwise TypeTest.scala does
// not typecheck. Todo: Figure out the reason for this difference.
object Test:

  type F[S, T] = (x: S) => Option[x.type & T]

  /** Trivial type test that always succeeds */
  def identity[T]: F[T, T] = Some(_)

  val x: 1 = 1
  val y = identity(x)
  val z: Option[1] = y


