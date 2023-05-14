def Test: Unit =
  val tup1 = 1 *: EmptyTuple
  val tup2 = 1 *: 2 *: EmptyTuple
  val tup3 = 1 *: 2 *: 3 *: EmptyTuple
  val tup4 = 1 *: 2 *: 3 *: 4 *: EmptyTuple
  val tup5 = 1 *: 2 *: 3 *: 4 *: 5 *: EmptyTuple
  val tup22 = 1 *: 2 *: 3 *: 4 *: 5 *: 6 *: 7 *: 8 *: 9 *: 10 *: 11 *: 12 *: 13 *: 14 *: 15 *: 16 *: 17 *: 18 *: 19 *: 20 *: 21 *: 22 *: EmptyTuple
  val tup23 = 1 *: 2 *: 3 *: 4 *: 5 *: 6 *: 7 *: 8 *: 9 *: 10 *: 11 *: 12 *: 13 *: 14 *: 15 *: 16 *: 17 *: 18 *: 19 *: 20 *: 21 *: 22 *: 23 *: EmptyTuple

  tup1._2 // error

  tup2._3 // error

  tup22._23 // error

  tup23._1 // error
  tup23._2 // error
  tup23._3 // error
  tup23._4 // error
  tup23._5 // error
  tup23._6 // error
  tup23._7 // error
  tup23._8 // error
  tup23._9 // error
  tup23._10 // error
  tup23._11 // error
  tup23._12 // error
  tup23._13 // error
  tup23._14 // error
  tup23._15 // error
  tup23._16 // error
  tup23._17 // error
  tup23._18 // error
  tup23._19 // error
  tup23._20 // error
  tup23._21 // error
  tup23._22 // error
  tup23._23 // error
