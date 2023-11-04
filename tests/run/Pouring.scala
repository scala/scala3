type Glass = Int
type Levels = Vector[Int]

class Pouring(capacity: Levels):

  enum Move:
    case Empty(glass: Glass)
    case Fill(glass: Glass)
    case Pour(from: Glass, to: Glass)

    def apply(levels: Levels): Levels = this match
      case Empty(glass) =>
        levels.updated(glass, 0)
      case Fill(glass) =>
        levels.updated(glass, capacity(glass))
      case Pour(from, to) =>
        val amount = levels(from) min (capacity(to) - levels(to))
        levels.updated(from, levels(from) - amount)
              .updated(to,   levels(to)   + amount)
  end Move

  val glasses = 0 until capacity.length
  val moves =
    (for g <- glasses yield Move.Empty(g))
    ++ (for g <- glasses yield Move.Fill(g))
    ++ (for g1 <- glasses; g2 <- glasses if g1 != g2 yield Move.Pour(g1, g2))

  class Path(history: List[Move], val endContent: Levels):
    def extend(move: Move) = Path(move :: history, move(endContent))
    override def toString = s"${history.reverse.mkString(" ")} --> $endContent"

  def from(paths: Set[Path], explored: Set[Levels]): LazyList[Set[Path]] =
    if paths.isEmpty then LazyList.empty
    else
      val extensions =
        for
          path <- paths
          move <- moves
          next = path.extend(move)
          if !explored.contains(next.endContent)
        yield next
      paths #:: from(extensions, explored ++ extensions.map(_.endContent))

  def solutions(target: Int): LazyList[Path] =
    val initialContent: Levels = capacity.map(_ => 0)
    val initialPath = Path(Nil, initialContent)
    for
      paths <- from(Set(initialPath), Set(initialContent))
      path <- paths
      if path.endContent.contains(target)
    yield path
end Pouring

@main def Test(target: Int, capacities: Int*) =
  val problem = Pouring(capacities.toVector)
  println(s"Moves: ${problem.moves}")
  println(s"Solution: ${problem.solutions(target).headOption}")
