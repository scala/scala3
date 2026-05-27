object Test {

  def main(args: Array[String]): Unit = {
    val r = complileMe
    println(r)
  }

  def complileMe: Int = {
    Macro.changeIndexWhere{
      val arr = Array(1,2,3)
      val result = arr.indexWhere(_ == 2)
      result
    }
  }

}
