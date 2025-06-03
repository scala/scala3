// https://github.com/scala/scala3/issues/1065
package hello

object world {
  def mkArray(atype: Int): Array[_ <: AnyVal] = {
    (if (atype == 1) new Array[Int](10) else new Array[Float](10))
  }

  def main(args: Array[String]): Unit = {
    println(mkArray(1))
  }
}
