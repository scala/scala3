package dotty.tools.scaladoc

class Searchbar {
  val pages = SearchbarGlobals.pages.toList.map(PageEntry.apply)
  val parser = QueryParser()
  val searchEngine = PageSearchEngine(pages)
  val inkuireEngine = InkuireJSSearchEngine()
  val component = SearchbarComponent(searchEngine, inkuireEngine, parser)
}