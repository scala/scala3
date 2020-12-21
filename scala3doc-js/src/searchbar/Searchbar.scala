package dotty.dokka

class Searchbar {
  val pages = SearchbarGlobals.pages.toList.map(PageEntry.apply)
  val engine = SearchbarEngine(pages)
  val component = SearchbarComponent(engine.query)
}
