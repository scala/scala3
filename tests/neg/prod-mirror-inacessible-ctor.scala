import scala.deriving.Mirror

package lib {
  sealed trait Foo
  object Foo // normally, would cache a mirror if one exists.
  case class Bar private[lib] () extends Foo
  case object Bar // force mirror for Bar to be anonymous.


  object CallSiteSucceed {
    val mFoo = summon[Mirror.SumOf[lib.Foo]] // ok
    val mBar = summon[Mirror.ProductOf[lib.Bar]] // ok
  }

}

package app {

  object MustFail {
    // we are outsite of accessible scope for Bar's ctor, so this should fail.

    val mFoo = summon[Mirror.SumOf[lib.Foo]] // error
    val mBar = summon[Mirror.ProductOf[lib.Bar]] // error
  }

}
