package dotty.tools.dottydoc
package js

import scala.scalajs.{ js => sjs }
import sjs.timers._
import org.scalajs.dom

object Search {
  private var isSearching = false

  def apply(input: dom.html.Input) = considerSearch(input)

  def considerSearch(input: dom.html.Input): dom.Event => Unit = { e =>
    val query = input.value
    if (query.length > 2) setTimeout(200) {
      if (!isSearching) {
        isSearching = true
        performSearch(query)
      }
    }
  }

  def performSearch(query: String): Unit = {
    println(s"searching for $query...")

    val matchingPackages = EntityIndex.packages.values.collect {
      case x if x.name.startsWith(query) => x
    }

    println("Found matching packages: ")
    matchingPackages.map(_.name).foreach(println)
    isSearching = false
  }
}
