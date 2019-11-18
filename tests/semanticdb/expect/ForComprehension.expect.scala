package example

class ForComprehension/*<-example::ForComprehension#*/ {
  for {
    a/*<-local0*/ <- List/*->scala::package.List.*//*->scala::collection::IterableFactory#apply().*/(1)/*->scala::collection::immutable::List#flatMap().*/
    b/*<-local1*//*->local1*/ <- List/*->scala::package.List.*//*->scala::collection::IterableFactory#apply().*/(1)/*->scala::collection::IterableOps#withFilter().*/
    if b/*->local1*/ >/*->scala::Int#`>`(+3).*/ 1/*->scala::collection::WithFilter#map().*/
    c/*<-local2*//*->local2*/ = a/*->local0*/ +/*->scala::Int#`+`(+4).*/ b/*->local1*//*->scala::collection::immutable::List#map().*/
  } yield (a/*->local0*/, b/*->local1*/, c/*->local2*/)
  for {
    a/*<-local4*/ <- List/*->scala::package.List.*//*->scala::collection::IterableFactory#apply().*/(1)/*->scala::collection::immutable::List#flatMap().*/
    b/*<-local5*/ <- List/*->scala::package.List.*//*->scala::collection::IterableFactory#apply().*/(a/*->local4*/)/*->scala::collection::IterableOps#withFilter().*/
    if (
      a/*->local4*/,
      b/*->local5*/
    ) ==/*->scala::Any#`==`().*/ (1, 2)/*->scala::collection::WithFilter#flatMap().*/
    (
      c/*<-local7*/,
      d/*<-local8*/
    ) <- List/*->scala::package.List.*//*->scala::collection::IterableFactory#apply().*/((a/*->local4*/, b/*->local5*/))/*->scala::collection::WithFilter#withFilter().*//*->scala::collection::IterableOps#withFilter().*/
    if (
      a/*->local4*/,
      b/*->local5*/,
      c/*->local7*/,
      d/*->local8*/
    ) ==/*->scala::Any#`==`().*/ (1, 2, 3, 4)/*->scala::collection::WithFilter#map().*/
    e/*<-local9*//*->local9*/ = (
      a/*->local4*/,
      b/*->local5*/,
      c/*->local7*/,
      d/*->local8*/
    )/*->scala::collection::IterableOps#withFilter().*/
    if e/*->local9*/ ==/*->scala::Any#`==`().*/ (1, 2, 3, 4)/*->scala::collection::WithFilter#flatMap().*/
    f/*<-local10*/ <- List/*->scala::package.List.*//*->scala::collection::IterableFactory#apply().*/(e/*->local9*/)/*->scala::collection::immutable::List#map().*/
  } yield {
    (
      a/*->local4*/,
      b/*->local5*/,
      c/*->local7*/,
      d/*->local8*/,
      e/*->local9*/,
      f/*->local10*/
    )
  }
}
