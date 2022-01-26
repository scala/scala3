object polyEtaExpand:
        
    def id[T](x: T) = x
    val valId5: Int => Boolean = id // error

    def wrongVariance1[T](x: T): Any = ???
    val valWrongVariance1: [T] => T => T = wrongVariance1 // error
    
    def wrongVariance2[T](x: Nothing): T = ???
    val valWrongVariance2: [T] => T => T = wrongVariance2 // error


