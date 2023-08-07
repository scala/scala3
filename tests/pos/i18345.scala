extension (vec: Seq[Int])
  def iterate[T](body: (() => Int) => T): T =
    val iterator = vec.iterator
    body(() => iterator.nextOption().getOrElse(0))

def withSequence[T](n: Int)(body: Seq[Int] => T): T =
  body((0 to n))

def test =

  withSequence(2):
    _.iterate: next =>
      next() + next() + next() + next()

  withSequence(2):
    _.iterate:
      next =>
        next() + next() + next() + next()

  withSequence(2): x =>
    x.iterate:
      next =>
        next() + next() + next() + next()

  withSequence(2): x =>
    x.iterate: next =>
      next() + next() + next() + next()

