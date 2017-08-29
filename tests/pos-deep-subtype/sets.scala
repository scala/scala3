object Test {

  val subPatBinders = List[Symbol]()

  def extraStoredBinders: Set[Symbol] = ???

  val storedBinders: Set[Symbol] =
    (if (true) subPatBinders.toSet else Set.empty) ++ extraStoredBinders// -- ignoredSubPatBinders


}
