object Main {
  def toClue[A](a: A): Clue[A] = Clue.generate(a)
  1.asdf // error
}