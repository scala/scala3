package example

object O1 extends A1 with A2 with A3 with A4 with A5 with A6:
  object O2:
    val someKey: Int = 42
    def getInfo: String = "info"

    object Nested:
      val flag: Boolean = true

    type MyType = String
    val myValue: MyType = "value"
end O1
