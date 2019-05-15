package scala.annotation

/** A method annotation that suggests that the annotated method should
 *  be used as an infix operator. Infix operations with alphanumeric
 *  operator names require the operator to be annotated with `@infix`.
 */
final class infix extends StaticAnnotation