// See https://github.com/scalameta/scalameta/issues/1749
package example

import scala.math.Ordered/*->scala::math::Ordered.*/.orderingToOrdered/*->scala::math::Ordered.orderingToOrdered().*/

class Issue1749/*<-example::Issue1749#*/ {
  val x1/*<-example::Issue1749#x1.*/ = 42
  val x2/*<-example::Issue1749#x2.*/ = 42
  (x1/*->example::Issue1749#x1.*/, x1/*->example::Issue1749#x1.*/)
    .compare/*->scala::math::Ordered#compare().*/((x2/*->example::Issue1749#x2.*/, x2/*->example::Issue1749#x2.*/))
}

class Issue1854/*<-example::Issue1854#*/ {
  val map/*<-example::Issue1854#map.*/ = collection.mutable.Map/*->scala::collection::mutable::Map.*/.empty/*->scala::collection::MapFactory.Delegate#empty().*/[String/*->scala::Predef.String#*/, String/*->scala::Predef.String#*/]
  map/*->example::Issue1854#map.*/("a") = "b"
}
