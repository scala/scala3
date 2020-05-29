package scala.annotation

/** Equivalent to declaring a super trait in Scala 3. This annotation
 *  should be used only for files that need to cross-compile between
 *  Scala 2 and 3.
 */
final class superTrait extends StaticAnnotation

