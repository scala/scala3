  opaque type CovariantArray[+A] = Array[A] // error

  object CovariantArray:
    def crash() =
      val stringArray: CovariantArray[String] = Array("foo", "bar")
      val anyArray: CovariantArray[Any] = stringArray
      anyArray(0) = 42
      stringArray(0).length

  @main def Test = CovariantArray.crash()