trait E[T]

class X {
  val e1: E[Int] = ???
  val e2: E[String] = ???
  val e3: E[List[Int]] = ???
  val e4: E[List[String]] = ???
  val e5: E[Double] = ???
  val e6: E[(String, String)] = ???
  val e7: E[(String, Int)] = ???
  val e8: E[(Int, List[String])] = ???
  val e9: E[Long] = ???
  val e10: E[(Long, Long)] = ???
  val e11: E[(Long, Long, Int)] = ???
  val e12: E[List[Long]] = ???
  val e13: E[List[Int]] = ???
  val e14: E[(String, String)] = ???
  val e15: E[(String, String, String)] = ???
  val e16: E[(Int, String)] = ???
  val e17: E[(String, Long, String)] = ???
  val e18: E[(Long, String, String)] = ???
  val e19: E[(String, String, Long)] = ???
  val e20: E[(String, Int, String)] = ???
  val e21: E[(Int, String, String)] = ???
  val e22: E[(String, String, Int)] = ???
  val e23: E[(String, String, Boolean)] = ???
  val e24: E[(Boolean, Boolean, String)] = ???
  val e25: E[(String, Int, Boolean)] = ???
  val e26: E[List[(String, String)]] = ???
  val e27: E[List[(Int, String)]] = ???
  val e28: E[List[(String, Int)]] = ???
  val e29: E[List[(Long, String)]] = ???
  val e30: E[List[(String, Long)]] = ???
  val e31: E[List[(Boolean, String)]] = ???
  val e32: E[List[(String, Boolean)]] = ???
  val e33: E[List[((String, String), String)]] = ???
  val e34: E[List[((String, Int), String)]] = ???
  val e35: E[List[((Long, String), String)]] = ???
  val e36: E[List[((Boolean, String), String)]] = ???
  val e37: E[List[((String, String), Int)]] = ???
  val e38: E[List[((String, String), (String, Int))]] = ???
  val e39: E[List[((Boolean, Long), (String, Int))]] = ???
  val e40: E[List[((Int, Long), (Boolean, Int))]] = ???
  val e41: E[List[((String, (Int, String)), (String, Int))]] = ???
  val e42: E[List[((Boolean, (Int, String)), (String, Int))]] = ???
  val e43: E[List[((String, (Int, String)), (Boolean, Int))]] = ???
  val e44: E[(Int, List[String], Long)] = ???
  val e45: E[(Int, List[Int], Long)] = ???
  val e46: E[(Int, List[Long], Long)] = ???
  val e47: E[(String, List[String], Long)] = ???
  val e48: E[(Int, List[String], Boolean)] = ???
  val e49: E[Char] = ???

  val all = List(
    e1,
    e2,
    e3,
    e4,
    e5,
    e6,
    e7,
    e8,
    e9,
    e10,
    e11,
    e12,
    e13,
    e14,
    e15,
    e16,
    e17,
    e18,
    e19,
    e20,
    e21,
    e22,
    e23,
    e24,
    e25,
    e26,
    e27,
    e28,
    e29,
    e30,
    e31,
    e32,
    e33,
    e34,
    e35,
    e36,
    e37,
    e38,
    e39,
    e40,
    e41,
    e42,
    e43,
    e44,
    e45,
    e46,
    e47,
    e48,
    e49
  )
}