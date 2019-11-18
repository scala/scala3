package example

class ForComprehension/*<-example::ForComprehension#*/ {
  for {
    a/*<-local0*/ <- List/*->scala::package.List.*//*->scala::collection::IterableFactory#apply().*/(1)/*->scala::collection::immutable::List#flatMap().*/
    b/*<-local1*//*->local1*/ <- List/*->scala::package.List.*//*->scala::collection::IterableFactory#apply().*/(1)/*->scala::collection::IterableOps#withFilter().*/
    if b/*->local1*/ >/*->scala::Int#`>`(+3).*/ 1/*->scala::collection::WithFilter#map().*/
    c/*<-local2*//*->local2*/ = a/*->local0*/ +/*->scala::Int#`+`(+4).*/ b/*->local1*//*->scala::collection::immutable::List#map().*/
  } yield (a/*->local0*/, b/*->local1*/, c/*->local2*/)
  for {
    a/*<-local3*/ <- List/*->scala::package.List.*//*->scala::collection::IterableFactory#apply().*/(1)/*->scala::collection::immutable::List#flatMap().*/
    b/*<-local4*/ <- List/*->scala::package.List.*//*->scala::collection::IterableFactory#apply().*/(a/*->local3*/)/*->scala::collection::IterableOps#withFilter().*/
    if (
      a/*->local3*/,
      b/*->local4*/
    ) ==/*->scala::Any#`==`().*/ (1, 2)/*->scala::collection::WithFilter#flatMap().*/
    (
 /*<-local7*/     c/*<-local5*/,
      d/*<-local6*/
    ) <- List/*->scala::package.List.*//*->scala::collection::IterableFactory#apply().*/((a/*->local3*/, b/*->local4*/))/*->scala::collection::WithFilter#withFilter().*//*->scala::collection::IterableOps#withFilter().*/
    if (
      a/*->local3*/,
      b/*->local4*/,
      c/*->local5*/,
      d/*->local6*/
    ) ==/*->scala::Any#`==`().*/ (1, 2, 3, 4)/*->scala::collection::WithFilter#map().*/
    e/*<-local8*//*->local7*//*->local8*/ = (
      a/*->local3*/,
      b/*->local4*/,
      c/*->local5*/,
      d/*->local6*/
    )/*->scala::collection::IterableOps#withFilter().*/
    if e/*->local8*/ ==/*->scala::Any#`==`().*/ (1, 2, 3, 4)/*->scala::collection::WithFilter#flatMap().*/
    f/*<-local9*/ <- List/*->scala::package.List.*//*->scala::collection::IterableFactory#apply().*/(e/*->local8*/)/*->scala::collection::immutable::List#map().*/
  } yield {
    (
      a/*->local3*/,
      b/*->local4*/,
      c/*->local5*/,
      d/*->local6*/,
      e/*->local8*/,
      f/*->local9*/
    )
  }
}
