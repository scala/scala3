object Test {
  ("a": "a") match { case a => Set.empty["a"] + a }
}
