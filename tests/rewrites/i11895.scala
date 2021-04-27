object test4 {

  class MyEnum

  given scala.util.CommandLineParser.FromString[MyEnum] with {
    def fromString(s: String): MyEnum = ???
  }
}