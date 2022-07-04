// scalajs: --skip

import repeatable._

@Plain_0(1)
@Plain_0(2)
@Plain_0(3)
trait U

@FirstLevel_0(Array(Plain_0(4), Plain_0(5)))
@FirstLevel_0(Array(Plain_0(6), Plain_0(7)))
trait T

object Test:
  def main(args: Array[String]) =
    val annValuesU = classOf[U].getAnnotation(classOf[FirstLevel_0]).value.toList.map(_.value).sorted
    annValuesU.foreach(println)

    println()

    val annValuesT = classOf[T].getAnnotation(classOf[SecondLevel_0]).value.toList.map(_.value.toList.map(_.value).sorted).sorted
    annValuesT.foreach(println)
