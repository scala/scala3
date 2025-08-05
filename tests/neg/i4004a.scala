@main def Test =
   "a".isInstanceOf[Null] // error
   null.isInstanceOf[Null] // error
   "a".isInstanceOf[Nothing] // error
   "a".isInstanceOf[Singleton] // error
   
   "a" match {
    case _: Singleton => () // error
    case _ => ()
   }
