package x

object Main:

   def testSimpleContext(): Unit =
     var x = 0
     val c = in1{ scope =>
       1
     }
