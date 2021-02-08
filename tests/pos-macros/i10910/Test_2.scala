import x.*

class Writer[A] {

   def awrite(a:A): CB[Unit] = ???

   inline def write(a:A):Unit =
     await(awrite(a))

}

class Reader[A] {

   def foreach_async(f: A=> CB[Unit]): CB[Unit] = ???

   def foreach(f: A=>Unit): Unit =
      foreach_async(v => CBM.pure(f(v)))

}

object Test {

 def main(args:Array[String]):Unit =
   val writer = new Writer[Int]()
   val reader = new Reader[Int]()
   val r = X.process{
       reader.foreach( v => writer.write(v) )
   }
   println("r")

}