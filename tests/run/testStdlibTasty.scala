@main def Test: Unit =
  assert(scala.StdlibBootstrappedDummy.thisMethodMustNotBePublished == "This method must not be published in a stable release of the library.")
