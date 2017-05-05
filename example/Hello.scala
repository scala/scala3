trait Foo(val i:Int)
object Hello extends Foo(5){
  def main(args: Array[String]): Unit = {
    println("Hello "+i)
//    println(new java.io.File(dotty.tools.StdLibSources.blacklistFile).getAbsolutePath)
//    println(new java.io.File(dotty.tools.StdLibSources.stdLibPath).getAbsolutePath)
  }
}
