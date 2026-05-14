// Regression test for the relaxation introduced in
// https://github.com/scala/scala3/pull/26034 .
//
// @odersky pointed out that if `T` is an abstract member type and `ip` is
// an inline parameter reference, then `ip.T` is not well formed (inline
// parameters are not stable paths in DOT), and that extending this
// relaxation would open soundness holes:
//
//   https://github.com/scala/scala3/pull/26034#discussion_r3240247347
//
// The fix in `matchAbstractTypeMember` admits inline-parameter `TermRef`s
// only when the refinement bounds line up — i.e. for the narrow case of
// inferring a wildcard refinement argument to the inline parameter's
// own abstract member. This file pins down the boundary by checking
// that all the *other* avenues that would treat `ip.T`/`ip.type` as a
// well-formed path are still rejected.

trait HasT { type T }

object HasT:
  type Aux[X] = HasT { type T = X }

// 1. Singleton-type formation on an inline parameter is rejected.
inline def singleton(inline h: HasT): h.type = h // error

// 2. A bare path-dependent type-member projection on an inline parameter
//    is rejected in formation contexts (parameter types, result types,
//    type ascription).
inline def projectResult(inline h: HasT): h.T = ??? // error

inline def projectParam(inline h: HasT, x: h.T): h.T = x // error // error

inline def projectAscription(inline h: HasT, x: Any): Unit =
  val _ = x.asInstanceOf[h.T] // error

// 3. The relaxed subtyping check does NOT admit a refinement match to an
//    arbitrary concrete `X` when the abstract member's bounds do not
//    permit it — the `matchAbstractTypeMember` bounds check still fires
//    (`String <:< h.T` fails because `h.T`'s lower bound is `Nothing`).
def expectAux(h: HasT.Aux[String]): Unit = ()
inline def coerceToAux(inline h: HasT): Unit = expectAux(h) // error

// 4. Two inline parameters do NOT collapse: `a.T` and `b.T` are distinct
//    path-dependent projections even when both are abstract, so a value
//    of `a.T` cannot be returned where `b.T` is expected. (This guards
//    against a future "ip is stable enough so any two ip.T are equal"
//    over-reach.)
class StrA extends HasT { type T = String }
class IntB extends HasT { type T = Int }

inline def conflate(inline a: HasT, inline b: HasT)(x: a.T): b.T = x // error // error
