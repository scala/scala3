package lib

import scala.deriving.Mirror

sealed trait Foo
object Foo // normally, would cache a mirror if one exists.
case class Bar private[lib] () extends Foo
case object Bar // force mirror for Bar to be anonymous.


object CallSiteSucceed {
  val mFoo = summon[Mirror.SumOf[Foo]] // ok
  val mBar = summon[Mirror.ProductOf[Bar]] // ok
  val sampleBar = Bar()
}
