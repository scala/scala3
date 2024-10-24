class annot[T](arg: T) extends scala.annotation.Annotation

type T1 = Int @annot(42)
type T2 = Int @annot("hello")
type T9 = Int @annot(classOf[Int])
type T3 = Int @annot(Array(1,2))
type T4 = Int @annot(Array(Array(1,2),Array(3,4)))
type T5 = Int @annot((1,2))
type T6 = Int @annot((1,2,3))
type T7 = Int @annot(((1,2),3))
type T8 = Int @annot(((1,2),(3,4)))

