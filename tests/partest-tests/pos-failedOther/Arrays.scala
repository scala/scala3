object Arrays{
  def main(a: Array[String]) = {
   val stringArr = new Array[String](1)
   stringArr(0) = "foo"
   val intArr = new Array[Int](1)
   stringArr(0) = intArr(0).toString
 }
}
