package dotty.dokka

class SearchbarEngine(pages: List[PageEntry]) {
  //TODO: Query should be parsed by QueryParser to list of filtering strategies called Matchers
  def query(query: String): List[PageEntry] = pages
}