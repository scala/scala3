package test

object parsePackage extends ParserTest {

  def test() = {
    val start = System.nanoTime()
    parseDir("/Users/odersky/workspace/dotty/src")
    parseDir("/Users/odersky/workspace/scala/src")
    val ms = (System.nanoTime() - start)/1000000
    println(s"$parsed files parsed in ${ms}ms")
  }

  def main(args: Array[String]): Unit = {
//    parse("/Users/odersky/workspace/scala/src/compiler/scala/tools/nsc/doc/model/ModelFactoryTypeSupport.scala")
    for (i <- 0 until 10) test()
  }
}