package dotty.tools.scaladoc

class Searchbar {
  val pages = SearchbarGlobals.pages.toList.map(PageEntry.apply)
  val engine = SearchbarEngine(pages)
  val parser = QueryParser()
  val component = SearchbarComponent(q => engine.query(parser.parse(q)))
}
