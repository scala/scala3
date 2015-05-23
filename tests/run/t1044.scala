object Test extends dotty.runtime.LegacyApp {
  val ducks = Array[AnyRef]("Huey", "Dewey", "Louie");
  ducks.iterator.asInstanceOf[Iterator[String]]
}
