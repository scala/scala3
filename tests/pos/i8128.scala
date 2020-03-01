object Test {
  def id: (x: 1 | 0) => x.type = x => x
  id(0): 0   // ok

  def id2: Function1[1 | 0, 1 | 0] {
    def apply(x: 1 | 0): x.type
  } = ???
  id2(0): 0  // ok

  def id3: Function1[1 | 0, Int] {
    def apply(x: 1 | 0): x.type
  } = ???
  id3(0): 0  // ok

  var x: 0 = 0
  var y: (1 & 0) | 0 = 0
  x = y
  y = x
}