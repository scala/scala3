trait Init[ScopeType]:
  sealed trait Initialize[A1]
  final class Bind[S, A1](val f: S => Initialize[A1], val in: Initialize[S])
    extends Initialize[A1]
  final class Value[A1](val value: () => A1) extends Initialize[A1]
  final class ValidationCapture[A1](val key: ScopedKey[A1], val selfRefOk: Boolean)
        extends Initialize[ScopedKey[A1]]
  final class TransformCapture(val f: [x] => Initialize[x] => Initialize[x])
        extends Initialize[[x] => Initialize[x] => Initialize[x]]
  final class Optional[S, A1](val a: Option[Initialize[S]], val f: Option[S] => A1)
        extends Initialize[A1]
  object StaticScopes extends Initialize[Set[ScopeType]]

  sealed trait Keyed[S, A1] extends Initialize[A1]
  trait KeyedInitialize[A1] extends Keyed[A1, A1]
  sealed case class ScopedKey[A](scope: ScopeType, key: AttributeKey[A]) extends KeyedInitialize[A]
  sealed trait AttributeKey[A]

abstract class EvaluateSettings[ScopeType]:
    protected val init: Init[ScopeType]
    import init._

    val transform: [A] => Initialize[A] => Unit = [A] =>
        (fa: Initialize[A]) =>
          fa match
            case k: Keyed[s, A]          => ???
            case b: Bind[s, A]           => ???
            case v: Value[A]             => ???
            case v: ValidationCapture[a] => ??? // unrearchable warning
            case t: TransformCapture     => ??? // unrearchable warning
            case o: Optional[s, A]       => ??? // unrearchable warning
            case StaticScopes            => ??? // unrearchable warning

