// https://github.com/scala/scala3/issues/24414
trait Constraint[A, C]:
  inline def test: Boolean

final class RuntimeConstraint[A, C](_test: Boolean)
object RuntimeConstraint:
  inline given fromConstraint[A, C](using c: Constraint[A, C]): RuntimeConstraint[A, C] =
    new RuntimeConstraint[A, C](c.test)

sealed trait Refined[A, C](using _rtc: RuntimeConstraint[A, C])

trait RefinedType[A, C] extends Refined[A, C]
