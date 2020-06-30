class Pouring(capacity: Vector[Int]):
  type Glass = Int
  type Content = Vector[Int]

  enum Move:
    def apply(content: Content): Content = this match
      case Empty(g) => content.updated(g, 0)
      case Fill(g) => content.updated(g, capacity(g))
      case Pour(from, to) =>
        val amount = content(from) min (capacity(to) - content(to))
        extension (s: Content) def adjust(g: Glass, delta: Int) = s.updated(g, s(g) + delta)
        content.adjust(from, -amount).adjust(to, amount)

    case Empty(glass: Glass)
    case Fill(glass: Glass)
    case Pour(from: Glass, to: Glass)
  end Move

  val moves =
    val glasses = 0 until capacity.length

       (for g <- glasses yield Move.Empty(g))
    ++ (for g <- glasses yield Move.Fill(g))
    ++ (for g1 <- glasses; g2 <- glasses if g1 != g2 yield Move.Pour(g1, g2))

  class Path(history: List[Move], val endContent: Content):
    def extend(move: Move) = Path(move :: history, move(endContent))
    override def toString = s"${history.reverse.mkString(" ")} --> $endContent"
  end Path

  val initialContent: Content = capacity.map(x => 0)
  val initialPath = Path(Nil, initialContent)

  def from(paths: Set[Path], explored: Set[Content]): LazyList[Set[Path]] =
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
    for
      paths <- from(Set(initialPath), Set(initialContent))
      path <- paths
      if path.endContent.contains(target)
    yield path
end Pouring

@main def Test =
  val problem = Pouring(Vector(4, 7))
  println(problem.moves)
  println(problem.solutions(6).head)
