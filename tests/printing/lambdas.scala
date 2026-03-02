object Main:
  val f1 = (x: Int) => x + 1
  val f2 = (x: Int, y: Int) => x + y
  val f3 = (x: Int) => (y: Int) => x + y
  val f4 = [T] => (x: Int) => x + 1
  val f5 = [T] => (x: Int) => (y: Int) => x + y
  val f6 = (x: Int) => { val x2 = x + 1; x2 + 1 }
  def f7(x: Int) = x + 1
  val f8 = f7
  val l = List(1,2,3)
  l.map(_ + 1)
  l.map(x => x + 1)
  l.map(x => { val x2 = x + 1; x2 + 1 })
  l.map(f7)
