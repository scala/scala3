package x


object Main {

 def main(args:Array[String]):Unit =
   val arr1 = new MyArr[Int]()
   val arr2 = new MyArr[Int]()
   val r = X.process{
       arr1.withFilter(x => x == await(CBM.pure(1)))
           .flatMap(x =>
              arr2.withFilter( y => y == await(CBM.pure(2)) ).
                map2( y => x + y )
           )
   }
   println(r)

}
