//> using options -language:experimental.specializedTraits

inline trait ListA[T: Specialized](vals: T*): 
    def printVals() = 
        vals.foreach(println(_))

@main def Test =
    val bungle: List[Short] = List(4123, 6, 7, 8, 10, 5, 11, 100)
    val x = new ListA[Short](bungle*) {}
    x.printVals()
