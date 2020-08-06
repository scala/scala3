package tests
package extensionMethodSignatures

class ClassOne
{
    // Commented cases won't work for now
    // extension ClassTwoOps on (c: ClassTwo):
    //     def getA() = c.a

    def (c: ClassTwo).getB(): String
         = c.b
        
    def (c: ClassTwo).getGivenParams(a: Int, b: Int, d: Int)(e: String): Int
        = 56

    def (c: ClassTwo).|||:(a: Int, b: Int, d: Int)(e: String): Int
        = 56
    // extension (c:ClassTwo):
    //     def getString()
    //          = c.toString()

    //     def getInt()
    //          = 5
}

case class ClassTwo(a: String, b: String)
{

}