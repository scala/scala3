
sealed trait Transformation[T]

case object Count extends Transformation[Int]
case class MultiTransformation[T1 <: Transformation[?], T2 <: Transformation[?]](t1: T1, t2: T2) // error cyclic
  extends Transformation[MultiTransformationResult[T1, T2]]

type MultiTransformationResult[T1 <: Transformation[?], T2 <: Transformation[?]] <: Tuple = (T1, T2) match {
  case (Transformation[t], MultiTransformation[t1, t2]) => t *: MultiTransformationResult[t1, t2]
  case (Transformation[t1], Transformation[t2]) => (t1, t2)
}
