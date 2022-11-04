package example

import deriving.Mirror

case class Foo private (i: Int)

// case object companion here prevents Foo from caching
// the mirror in its companion, so all potential mirrors for Foo will be anonymous.
case object Foo

// however we can not provide an anonymous mirror
// at this call site because the constructor is not accessible.
val mFoo = summon[Mirror.Of[Foo]] // error: no mirror found
