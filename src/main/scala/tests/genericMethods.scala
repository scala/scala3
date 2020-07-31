package tests.genericMethods

class Types:
    def orTypes(base: Int | String, nested: List[Int | Double] | Seq[String]): Unit 
    = ???
    def andTypes(base: Int & String, nested: List[Int & Double] & Seq[String]): Unit 
    = ???
    
    def mixedAndAndOr(base: Int & String, nested: List[Int | Double] & Seq[String]): Unit 
    = ???

    def literal(i: 1, d: 3.3, c: Char): 34 
    = 34 /* TODO: #34 Deleted type signature 'c' to repair searchbar until fix is merged */
    
    def byName(a: => Int, b: => String | Int): Unit 
    = ???