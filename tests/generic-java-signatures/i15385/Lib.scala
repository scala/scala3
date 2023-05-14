class Loc(val idx: Int) extends AnyVal

class Foo:
  def testNoParam[A <: Int]: A = 1.asInstanceOf[A]
  def testSingleParam[A <: Int](a: A): A = 2.asInstanceOf[A]        // <A:Ljava/lang/Object;>(I)I
  def testSingleParam2[A <: Int](a: A): Box[A] = new Box[A](a)      // <A:Ljava/lang/Object;>(I)LBox<Ljava/lang/Object;>;
  def testSingleParam3[A <: Int](box: Box[A]): A = box.value         // <A:Ljava/lang/Object;>(LBox<Ljava/lang/Object;>;)I
  def testOtherReturn[A <: Int](a: A): String = "3"
  def testNoErasure[A <: String](a: A): A = "4".asInstanceOf[A]
  def testMultiParam[A <: Int, B <: String](a: A, b: B): A = 5.asInstanceOf[A]

  def testVCNoParam[A <: Loc]: A = Loc(1).asInstanceOf[A]
  def testVCSingleParam[A <: Loc](a: A): A = Loc(2).asInstanceOf[A]
  def testVCOtherReturn[A <: Loc](a: A): String = "3"
  def testVCNoErasure[A <: String](a: A): A = "4".asInstanceOf[A]
  def testVCMultiParam[A <: Loc, B <: String](a: A, b: B): A = Loc(5).asInstanceOf[A]

class Box[T](val value: T)

class BarParent[X, Y]
trait BarInterface[F, G]
abstract class Bar[A <: Int](a: A) extends BarParent[A, String] with BarInterface[Int, A]:
  def getMap: Map[String, A]
  def bar[B](a: A, b: B): (A, B, Int)
