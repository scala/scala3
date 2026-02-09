package scala.annotation

/** An annotation that can be used to mark a mutable field as trackable for nullability.
 *  With explicit nulls, a normal mutable field cannot be tracked for nullability by flow typing,
 *  since it can be updated to a null value at the same time.
 *  This annotation will force the compiler to track the field for nullability, as long as the
 *  prefix is a stable path.
 *  See `tests/explicit-nulls/pos/force-track-var-fields.scala` for an example.
 */
private[scala] final class stableNull extends StaticAnnotation
