package x


object Main {

 def main(args:Array[String]):Unit =
   val arr = new MyArr[Int]()
   val r = X.process{
       arr.map1( zDebug =>
         await(CBM.pure(1).map(a => zDebug + a))
       )
   }
   println("r")

}