trait Fail8 {
  val ignoredSubPatBinders: Set[Int]
  val storedBinders: Set[Int] =
    (if (true) List.empty.toSet else Set.empty) ++ extraStoredBinders -- ignoredSubPatBinders
  val extraStoredBinders: Set[Int]
}