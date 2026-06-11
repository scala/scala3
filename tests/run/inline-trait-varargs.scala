inline trait ListA[T](vals: T*): 
    def printVals() = 
        vals.foreach(println(_))

@main def Test =
    val x = new ListA[Short](4, 6, 7, 8, 10, 5, 11, 100) {}
    x.printVals()
