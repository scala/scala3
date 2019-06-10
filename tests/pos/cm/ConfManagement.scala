package cm

case class Person(name: String)
case class Paper(title: String, authors: List[Person], body: String)

class Viewers(val persons: Set[Person])

class ConfManagement(papers: List[Paper], realScore: Map[Paper, Int]) {

  private def hasConflict(ps1: Set[Person], ps2: Iterable[Person]) =
    ps2.exists(ps1 contains _)

  def viewers(implicit vs: Viewers): Set[Person] =
    vs.persons

  def score(paper: Paper)(implicit vs: Viewers): Int =
    if (hasConflict(viewers, paper.authors)) -100
    else realScore(paper)

  def viewRankings(implicit vs: Viewers): List[Paper] =
    papers.sortBy(score(_))

  def delegateTo[T](query: Viewers => T, p: Person)(implicit vs: Viewers): T =
    query(new Viewers(viewers + p))
}