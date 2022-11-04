import deriving.Mirror

package example {
  case class Foo private [example] (val i: Int)

  // case object companion here prevents Foo from caching
  // the mirror in its companion, so all potential mirrors for Foo will be anonymous.
  case object Foo

}

class Bar extends example.Foo(23) { // error // error: cant access private[example] ctor

  // however we can not provide an anonymous mirror
  // at this call site because the constructor is not accessible.
  val mFoo = summon[Mirror.Of[example.Foo]] // error: no mirror

}
