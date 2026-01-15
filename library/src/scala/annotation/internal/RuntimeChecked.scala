package scala.annotation.internal

import language.experimental.captureChecking

import scala.annotation.Annotation
import scala.annotation.experimental

/**An annotation marking an intention that all checks on a value can be reliably performed at runtime.
 *
 * The compiler will remove certain static checks except those that can't be performed at runtime.
 */
final class RuntimeChecked() extends Annotation
