import generic.*
import Tree.*
import List.*
import java.io.*
import Shapes.*
import SearchResult.*

object Test {
  import Serialization.*

  private var lCount, tCount, sCount = 0

// ------- Code that will eventually be produced by macros -------------

  implicit def ListSerializable[Elem](implicit es: Serializable[Elem]): Serializable[List[Elem]] = {
    implicit lazy val lsElem: Serializable[List[Elem]] = {
      lCount += 1 // test code to verify we create bounded number of Serializables
      RecSerializable[List[Elem], List.Shape[Elem]]
    }
    lsElem
  }

  implicit def TreeSerializable[R]: Serializable[Tree[R]] = {
    implicit lazy val tR: Serializable[Tree[R]] = {
      tCount += 1 // test code to verify we create bounded number of Serializables
      RecSerializable[Tree[R], Tree.Shape[R]]
    }
    tR
  }
  implicit lazy val tsInt: Serializable[Tree[Int]] = TreeSerializable[Int]
  implicit lazy val tsBoolean: Serializable[Tree[Boolean]] = TreeSerializable[Boolean]

  implicit lazy val SearchResultSerializable: Serializable[SearchResult] = {
    sCount += 1
    RecSerializable[SearchResult, SearchResult.Shape]
  }

// ------- Test code --------------------------------------------------------

  /** Serialize data, then deserialize it back and check that it is the same. */
  def sds[D](data: D)(implicit ser: Serializable[D]) = {
    val outBytes = new ByteArrayOutputStream
    val out = new DataOutputStream(outBytes)
    ser.write(data, out)
    out.flush()
    val inBytes = new ByteArrayInputStream(outBytes.toByteArray)
    val in = new DataInputStream(inBytes)
    val result = ser.read(in)
    assert(data == result, s"$data != $result")
  }

  val data1 =
    Cons(1, Cons(2, Cons(3, Nil)))

  val data2 =
    If(IsZero(Pred(Succ(Zero))), Succ(Succ(Zero)), Pred(Pred(Zero)))

  val data3 = Cons(Color.Red, Cons(Color.Green, Cons(Color.Blue, Nil)))

  val data4 = Ambiguous(Success(Color.Green), Diverging)

  def main(args: Array[String]) = {
    sds(data1)
    assert(lCount == 1, lCount)
    sds(data2)
    assert(tCount == 2, tCount)
    sds(data3)
    assert(lCount == 2, lCount)
    sds(data4)
    assert(sCount == 1, sCount)
  }
}
