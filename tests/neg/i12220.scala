val a: List[Any] = List(List(1,2), List(3,4))
val _ = for(b <- a ; c <- b.asInstanceOf[List]) { println(c) } // error

