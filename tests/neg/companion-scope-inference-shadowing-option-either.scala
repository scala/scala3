// Shadowing counterpart of `tests/pos/companion-inference-option-either.scala`.
// `Some(Red)` with target `Option[Color]` normally infers `A = Color` and
// then resolves `Red` via Color's companion. If a local `Red` of wrong
// type is in scope, normal resolution wins and the call's type parameter
// is inferred from the local's type instead, producing a type mismatch
// at the outer val.
import scala.language.experimental.companionScopeInference

object OptionEitherShadow:

  sealed trait Color
  object Color:
    case object Red extends Color

  val Red = 42

  val o: Option[Color] = Some(Red) // error
