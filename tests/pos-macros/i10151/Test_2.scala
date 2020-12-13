package x

object Main {

 def main(args:Array[String]):Unit =
   val arr = new MyArr[Int,Int]()
   val r = X.process{
       arr.map1( (x,y) =>
         ( 1, await(CBM.pure(x)) )
       )
   }
   println("r")

}
