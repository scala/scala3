object stuff with
  def exec(dir: Int) = ???

extension on (a: Int) with
  inline def exec: Unit = stuff.exec("aaa") // error
