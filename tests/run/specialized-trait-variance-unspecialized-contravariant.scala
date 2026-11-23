//> using options -language:experimental.specializedTraits

class Paper:
  override def toString = "paper"

inline trait Bin[-T: Specialized]:
  def throwAway(x: T): String = s"threw away $x"

def throwAwayInt(bin: Bin[Int]): String = bin.throwAway(100)
def throwAwayPaper(bin: Bin[Paper]): String = bin.throwAway(Paper())

@main def Test =
  assert(throwAwayInt(new Bin[Any] {}) == "threw away 100")
  assert(throwAwayInt(new Bin[AnyVal] {}) == "threw away 100")
  assert(throwAwayInt(new Bin[Int] {}) == "threw away 100")
  assert(throwAwayPaper(new Bin[Object] {}) == "threw away paper")
  assert(throwAwayPaper(new Bin[Any] {}) == "threw away paper")
  assert(throwAwayPaper(new Bin[Paper] {}) == "threw away paper")
