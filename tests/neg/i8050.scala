object stuff:
  def exec(dir: Int) = ???

extension on (a: Int):
  inline def exec: Unit = stuff.exec("aaa") // error
