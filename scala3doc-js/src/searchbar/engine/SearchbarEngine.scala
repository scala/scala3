package dotty.dokka

import math.Ordering.Implicits.seqOrdering

class SearchbarEngine(pages: List[PageEntry]) {
  def query(query: List[Matchers]): List[PageEntry] = {
    pages.map(p => p -> query.map(_(p))).filterNot(_(1).exists(_ < 0)).sortBy(_(1)).map(_(0))
  }
}