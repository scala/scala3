package scala.annotation
package internal

import language.experimental.captureChecking

/** An annotation carrying the skolem index allocated for an unstable
 *  expression that participates in qualified-type substitution. Attached
 *  either:
 *   - on a tree, via `Annotated(arg, @QualifierSkolemIndex(n))`, when the
 *     typer wraps a call's unstable argument so that `substParamInQualifiers`
 *     can recover the same skolem identity across TASTy round-trips;
 *   - on a val symbol, when `avoidRefs` lifts a local val into a skolem in
 *     a qualifier that has to escape the val's scope.
 *
 *  The annotation is informational only; it does not affect typing or
 *  subtyping, and it is stripped by erasure.
 */
class QualifierSkolemIndex(index: Int) extends StaticAnnotation
