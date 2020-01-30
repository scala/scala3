object Test {
  def id: (x: 1 | 0) => x.type = x => x
  id(0): 0
}