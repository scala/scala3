package x

object VP1:

   ///*
   def allocateServiceOperator(optInUsername: Option[String]): CB[Unit] = Async.transform { // error
      val username = optInUsername match
         case None =>
            while(false) {
               val nextResult = await(op1())
               val countResult = await(op1())
            }
         case Some(inUsername) =>
            val x = await(op1())
            inUsername
   }
   //*/

   def op1(): CB[String] = ???
