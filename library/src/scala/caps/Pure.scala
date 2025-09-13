package scala
package caps

import language.experimental.captureChecking

/** A marker trait that declares that all inheriting classes are "pure" in the
 *  sense that their values retain no capabilities including capabilities needed
 *  to perform effects. This has formal meaning only under capture checking.
 *
 *  NOTE: If an upper bound is Pure, we check that an argument conforms to it only
 *  in sources where capture checking is enabled. For instance,
 *
 *    def f[C <: Pure]()
 *    f[Object]()
 *
 *  would give an error only under capture checking. Pure is also dropped in
 *  upper bounds if it forms part of an &-type, or is under a type lambda.
 */
transparent trait Pure extends Any:
  this: Pure =>
