package benchmark

import scala.collection.{IterableFactoryDefaults, IterableOps, mutable}
import scala.collection.immutable.SeqOps
import scala.collection.mutable.{ArrayBuffer, ReusableBuilder}

class SeqPoints(pts: Seq[IdentityPoint]) extends Seq[IdentityPoint] with SeqOps[IdentityPoint,Seq,SeqPoints] with IterableOps[IdentityPoint,Seq,SeqPoints]{
  protected var arr : Array[Double] =  pts.flatMap(p => Array(p.x, p.y, p.z)).toArray
  final class SeqPtsBuilder extends mutable.ReusableBuilder[IdentityPoint, SeqPoints] {
    protected var elems : Array[IdentityPoint] = new Array[IdentityPoint](0) //make this array of doubles
    protected var capacity : Int = 0
    protected var size : Int = 0 //current number of elements
    private final val DefaultInitialSize = 16
    final val VM_MaxArraySize = 2147483639

    private def resize(size : Int) : Unit = {
//        println("resize: making array with size:" + size)
      elems = mkArray(size)
//        println("resize: after resize, elems size:" + elems.length)
      capacity = size
      //Thread.sleep(2000)
    }

    private def ensureSize(size: Int) : Unit = {
      def aux(arrayLen : Int, targetLen: Int): Int = {
        if (targetLen < 0) throw new Exception(s"Overflow while resizing array of array-backed collection. Requested length: $targetLen; current length: $arrayLen; increase: ${targetLen - arrayLen}")
        else if (targetLen <= arrayLen) -1
        else if (targetLen > VM_MaxArraySize) throw new Exception(s"Array of array-backed collection exceeds VM length limit of $VM_MaxArraySize. Requested length: $targetLen; current length: $arrayLen")
        else if (arrayLen > VM_MaxArraySize / 2) VM_MaxArraySize
        else {
          val ret = math.max(targetLen,
            math.max(
              arrayLen * 2,
              DefaultInitialSize))
//            println(ret)
          ret
        }

      }
      val newLen = aux(capacity, size)
//        println("ensuresize: newLen: " + newLen)
      if (newLen > 0) resize(newLen)
    }

    override def addOne(elem: IdentityPoint): SeqPtsBuilder.this.type = {
//        println("addOne:adding" + elem + " to " + elems.mkString("Array(", ", ", ")"))
      ensureSize(size + 1)
//        println("addone: after ensureSize, size:" + size + ": " + elems.length)
//        println("addone: after ensuresize elems:" + elems.mkString("Array(", ", ", ")"))
      elems(size) = elem
//        println("his")
      size += 1
      this
    }

    override def clear(): Unit = size = 0

    private def mkArray(size: Int): Array[IdentityPoint] = {
      val newelems = new Array[IdentityPoint](size)
//        println("newelems init size:" + newelems.length)
//        println("newelems:" + newelems.mkString("Array(", ", ", ")"))
      if (this.size > 0) {
        Array.copy(elems, 0, newelems, 0, this.size)
      }
//        println("in mkarray-else: newelems has size:" + size + "newelems:" + newelems.length)
      newelems
    }

    override def result(): benchmark.SeqPoints = {
      if (capacity != 0 && capacity == size){
        capacity = 0
        val ret = elems
        elems = null
        SeqPoints(ret.toSeq)
      } else {
        SeqPoints(mkArray(size).toSeq)
      }
    }
  }

  override def fromSpecific(coll: IterableOnce[IdentityPoint]): SeqPoints = {
    val builder = newSpecificBuilder
    builder.sizeHint(coll, delta = 0)
    builder ++= coll
    builder.result()
  }

//  def fromSpecific(it: IterableOnce[A]): Array[A] = {
//    val b = newBuilder
//    b.sizeHint(it, delta = 0)
//    b ++= it
//    b.result()
//  }
  override def newSpecificBuilder: mutable.Builder[IdentityPoint, SeqPoints] = new SeqPtsBuilder

  override def empty: SeqPoints = new SeqPoints(Seq())

  private def round(v: Double): Double = (v * 100).toInt / 100.0


  override def apply(i: Int): IdentityPoint = {
    //if debug then println("apply: new point ")
    new IdentityPoint(arr(i * 3), arr(i * 3 + 1), arr(i * 3 + 2))
  }

  override def length: Int = {
    //if debug then println("length!")
    arr.length / 3
  }
  override def iterator: Iterator[IdentityPoint] = {
    //if debug then println("iterator")
    new SeqPtsIterator(arr, length)
  }

  override def toString(): String = {
    def aux(array : List[Double]) : String = {
      array match {
        case x :: y :: z :: res => s", Point(${round(x)}, ${round(y)}, ${round(z)})" + aux(res)
        case _ => ""
      }
    }
    aux(arr.toList)
  }
  class SeqPtsIterator(a: Array[Double], private var arrLen : Int) extends Iterator[IdentityPoint] {
    private var i = 0;
    override def hasNext: Boolean = arrLen > i
    def next(): IdentityPoint = {
      if (i == arrLen) Iterator.empty.next()
      val x = a(i * 3)
      val y = a(i * 3 + 1)
      val z = a(i * 3 + 2)
      i+=1
      IdentityPoint(x, y, z)
    }
  }
}

