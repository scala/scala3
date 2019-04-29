object Test extends App {
  class Config(val t1: Int)

  inline def m(t2:Int) = t2

  m(new Config(3).t1)
}