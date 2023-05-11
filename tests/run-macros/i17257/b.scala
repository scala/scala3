package derivation {
  //file b.scala
  val test = Helpers.summonAllOptimized[(
      ValueOf["a"], ValueOf["a"], ValueOf["a"], ValueOf["a"], ValueOf["a"],
      ValueOf["a"], ValueOf["a"], ValueOf["a"], ValueOf["a"], ValueOf["a"],
      ValueOf["a"], ValueOf["a"], ValueOf["a"], ValueOf["a"], ValueOf["a"],
      ValueOf["a"], ValueOf["a"], ValueOf["a"], ValueOf["a"], ValueOf["a"],
      ValueOf["a"], ValueOf["a"], ValueOf["a"] //Commenting out the last one here fixes the compile error
    )]
  val test2 = Helpers.summon23[ValueOf["a"]]
}
@main def Test =
  def assertions(list: List[ValueOf["a"]]): Unit =
    assert(list.size == 23)
    assert(list.map(_.value) == List(
      "a", "a", "a", "a", "a",
      "a", "a", "a", "a", "a",
      "a", "a", "a", "a", "a",
      "a", "a", "a", "a", "a",
      "a", "a", "a"
    ))
  assertions(derivation.test.toList)
  assertions(derivation.test2.toList)
