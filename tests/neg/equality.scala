object equality {

  case class Str(str: String)

  case class Num(x: Int)

  case class Other(x: Int)

  trait Option[+T]
  case class Some[+T](x: T) extends Option[T]
  case object None extends Option[Nothing]

  implicit def eqStr: Eql[Str, Str] = Eql.derived
  implicit def eqNum: Eql[Num, Num] = Eql.derived
  implicit def eqOption[T, U](implicit e: Eql[T, U]): Eql[Option[T], Option[U]] = Eql.derived

  case class PString(a: String) extends Proxy {
    def self = a
  }

/*
  implicit def eqString: Eql[String, String] = Eql.derived
  implicit def eqInt: Eql[Int, Int] = Eql.derived
  implicit def eqNumber: Eql[Number, Number] = Eql.derived
  implicit def eqIntNumber: Eql[Int, Number] = Eql.derived
  implicit def eqNumberInt: Eql[Number, Int] = Eql.derived
*/
  def main(args: Array[String]): Unit = {
    Some(Other(3)) == None

    val x = Str("abc")
    x == x

    val n = Num(2)
    val m = Num(3)
    n == m

    val so: Object = "abc"
    so == "abc"
    "abc" == so

    Other(1) == Other(2)

    Some(x) == None
    Some(x) == Some(Str(""))
    val z: Option[Str] = Some(Str("abc"))
    z == Some(x)
    z == None
    Some(x) == z
    None == z
    None == Some(Other(3))

    Other(3) == null
    Str("x") == null
    null == Other(3)
    null == Str("x")
    null == null

    1 == true  // error

    null == true // error
    true == null // error
    null == 1    // error
    1 == null    // error


    class Fruit derives Eql

    class Apple extends Fruit
    class Pear extends Fruit
    val a = new Apple
    val p = new Pear
    val f: Fruit = a
    a == p
    p == a
    f == p
    p == f
    Some(new Apple) == Some(new Pear)

    val i = 3
    val bi = BigInt(i)
    i == i
    bi == bi
    i == bi
    bi == i

    val ps = PString("hello")
    ps == "world"

    n match {
      case None =>   // error
    }

    x == Other(1)  // error
    Other(2) == x // error
    Other(1) == z // error
    z == Other(1) // error
    n == None // error

    Some(new Apple) == Some(Str("xx")) // error
    x == n  // error
    n == x  // error
    z == Some(n) // error
    z == n // error
    Some(n) == z // error
    Some(n) == Some(Other(3)) // error
    Some(Other(3)) == Some(n) // error
    n == z // error
    "abc" == 1 // error
    1 == "abc" // error
    "abc" == bi // error
    bi == "abc" // error
    "world" == ps // error

    val s1 = Set(1, 2, 3)
    val s2 = Set()

    Nil == s1 // error
    s1 == Nil // error
    Nil == s2 // error
    s2 == Nil // error

    import collection.parallel._
    val p1 = ParSeq(1, 2, 3)
    val p2 = ParSeq()
    Nil == p1 // OK
    p1 == Nil // OK
    Nil == p2 // OK
    p2 == Nil // Ok

  }
}
