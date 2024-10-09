package scala.annotation
package internal

/** An annotation that marks a capture ref as a reach capability occurring
 *  under a @use part of a parameter type.
 *  Example: When it occurs in `C[x* @use]`,  `x*` is encoded as
 *  `x.type @reachUnderuseCapability`
 */
class reachUnderUseCapability extends StaticAnnotation
