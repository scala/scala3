package x


object Main {


 def main(args:Array[String]):Unit =
   val r = X.transform{
     SlowPath.sum(1)
   }

}
