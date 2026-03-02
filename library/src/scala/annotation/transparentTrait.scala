package scala.annotation

import language.experimental.captureChecking

/** An annotation that can be used from Scala 2 to mark a trait as transparent.
 *  Scala 3 code would use the modifier `transparent` instead. Transparent traits
 *  are not inferred when combined with other types in an intersection.
 *  See reference/other-new-features/transparent-traits.html for details.
 */
@deprecated(message = "Transparent traits/classes via annotations is no longer supported. Use instead the `transparent` modifier", since = "3.8.0")
final class transparentTrait extends StaticAnnotation
