object Broken {
  private var map = Map[Class[?], String]()

  def addToMap(c : Class[?], s : String) = map += (c -> s)
  def fetch(c : Class[?]) = map(c)
}

object Works {
  private var map = Map[Class[?], String]()

  def addToMap(c : Class[?], s : String) = map += ((c, s))
  def fetch(c : Class[?]) = map(c)
}

object Works2 {
  private var map = Map[Class[?], String]()

  def addToMap(c : Class[?], s : String) = map += ((c : Class[?]) -> s)
  def fetch(c : Class[?]) = map(c)
}
