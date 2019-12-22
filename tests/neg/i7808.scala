object A {
    {
        val a: Int = 1
        object a {  // error
            this match {
                case _ => ()
            }
        }
    }
}