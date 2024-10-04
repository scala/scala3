class Foo:
  given Conversion[String, Data]:
    def apply(str: String): Data = new Data(str)

  class Data(str: String):
    def |(str: String) = new Data(this.str + str)

class Bar extends Foo:
  "str" | "ing"
