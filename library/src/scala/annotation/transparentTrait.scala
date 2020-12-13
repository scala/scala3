package scala.annotation

/** An annotation that can be used from Scala 2 to mark a trait as transparent.
 *  Scala 3 code would use the modifier `transparent` instead. Transparent traits
 *  are not inferred when combined with other types in an intersection.
 *  See reference/other-new-features/transparent-traits.html for details.
 */
final class transparentTrait extends StaticAnnotation
