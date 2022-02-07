package dotty.tools.scaladoc

import math.Ordering.Implicits.seqOrdering
import org.scalajs.dom.Node

class SearchbarEngine(pages: List[PageEntry]):
  def query(query: List[Matchers]): List[PageEntry] =
    pages
      .map( page =>
        page -> query.map(matcher => matcher(page))
      )
      .filterNot {
        case (page, matchResults) => matchResults.exists(_ < 0)
      }
      .sortBy {
        case (page, matchResults) => matchResults
      }
      .map {
        case (page, matchResults) => page
      }
