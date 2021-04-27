package tests
package extensionMethodSignatures

class ClassOne
{
  // Commented cases won't work for now
  // extension ClassTwoOps on (c: ClassTwo)
  //     def getA() = c.a
  extension (c: ClassTwo)
    def getB(): String
      = c.b

  extension (c: ClassTwo)
    def getGivenParams(a: Int, b: Int, d: Int)(e: String): Int
      = 56

  extension (c: ClassTwo)
    def |||:(a: Int)(b: Int, d: Int)(e: String): Int
      = 56
    def ++:(a: Int): Int
      = 45

  extension (b: Int)
    def secondGroup(): String
      = ???

  extension (c:ClassTwo)

    def getString(a: String): String
       = a

    def getInt(): Int
       = 5

  extension (s: String)
    def someMethod(): String
      = ???
    def otherMethod(a: Int): Int
      = ???
}

case class ClassTwo(a: String, b: String)
{

}

class ClassOneTwo extends ClassOne