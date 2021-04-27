object foo {
  trait ShapeLevel
  trait FlatShapeLevel extends ShapeLevel
  trait ColumnsShapeLevel extends FlatShapeLevel
  abstract class Shape[Level <: ShapeLevel, -Mixed, Unpacked, Packed]
  object Shape extends TupleShapeImplicits
  trait TupleShapeImplicits {
    implicit final def tuple2Shape[Level <: ShapeLevel, M1,M2, U1,U2, P1,P2](
      implicit u1: Shape[_ <: Level, M1, U1, P1], u2: Shape[_ <: Level, M2, U2, P2])
      : Shape[Level, (M1,M2), (U1,U2), (P1,P2)] = ???
  }
}
object Test {
  import foo.*
  implicit val shape: Shape[_ <: FlatShapeLevel, Int, Int, _] = null
  def hint = Shape.tuple2Shape(shape, shape)
  val hint2: foo.Shape[foo.FlatShapeLevel, (Int, Int), (Int, Int), _] = hint
}
