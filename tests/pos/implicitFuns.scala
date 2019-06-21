package cm2

case class Person(name: String)
case class Paper(title: String, authors: List[Person], body: String)

class Viewers(val persons: Set[Person])

class ConfManagement(papers: List[Paper], realScore: Map[Paper, Int]) extends App {

  private def hasConflict(ps1: Set[Person], ps2: Iterable[Person]) =
    ps2.exists(ps1 contains _)

  type Viewable[T] = given Viewers => T

  def vs: Viewable[Viewers] = implicitly

  def viewers: Viewable[Set[Person]] = vs.persons

  def score: Paper => Viewable[Int] =
    paper =>
      if hasConflict(viewers, paper.authors) then -100
      else realScore(paper)

  def viewRankings: Viewable[List[(String, Int)]] =
    papers.sortBy(-score(_)).map(p => (p.title, score(p)))

  def delegateTo[T]: (Viewers => T) => Person => Viewable[T] =
    query => p => query(new Viewers(viewers + p))
}

object Test extends App {
  def bob = Person("Bob")
  def peter = Person("Peter")
  def p1 = Paper("Bob's paper", List(bob), "")
  def p2 = Paper("Peter's paper", List(peter), "")

  implicit def __1: Viewers = new Viewers(Set(bob))

  val cm = new ConfManagement(List(p1, p2), Map(p1 -> 2, p2 -> 3))

  println(cm.viewRankings)
  println(cm.score(p1))
  println(Orderings.isLess(Nil)(List(1, 2, 3)))
}

object Orderings extends App {

  trait Ord[T] { def less: T => T => Boolean }

  implicit def __1: Ord[Int] = new Ord[Int] {
    def less: Int => Int => Boolean =
      x => y => x < y
  }

  implicit def __2[T]: given Ord[T] => Ord[List[T]] = new Ord[List[T]] {
    def less: List[T] => List[T] => Boolean =
      xs => ys =>
        if ys.isEmpty then false
        else if xs.isEmpty then true
        else if xs.head == ys.head then less(xs.tail)(ys.tail)
        else isLess(xs.head)(ys.head)
  }

  def isLess[T]: T => T => given Ord[T] => Boolean =
    x => y => implicitly[Ord[T]].less(x)(y)

  println(isLess(Nil)(List(1, 2, 3)))
  println(isLess(List(List(1)))(List(List(1))))
}
