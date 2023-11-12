import language.experimental.captureChecking
import collection.{View, Seq}
import collection.mutable.{ArrayBuffer, ListBuffer}

import java.io.*

object Test0:

  def usingLogFile[T](op: FileOutputStream^ => T): T =
    val logFile = FileOutputStream("log")
    val result = op(logFile)
    logFile.close()
    result

  def test(xs: List[Int]) =
    usingLogFile: f =>
      xs.map: x =>
        f.write(x)
        x * x

object Test1:
  def test(it: Iterator[Int]^, v: View[Int]^) =
    val isEven: Int => Boolean = _ % 2 == 0
    val it2 = it.filter(isEven)
    val _: Iterator[Int]^{it, isEven} = it2
    val it2c: Iterator[Int]^{it2} = it2
    val v2 = v.filter(isEven)
    val _: View[Int]^{v, isEven} = v2
    val v2c: View[Int]^{v2} = v2
    val v3 = v.drop(2)
    val _: View[Int]^{v} = v3
    val v3c: View[Int]^{v3} = v3
    val (xs6, xs7) = v.partition(isEven)
    val (xs6a, xs7a) = v.partition(_ % 2 == 0)
