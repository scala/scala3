package example

class ForComprehension/*<-example::ForComprehension#*/ {
  for {
    a/*<-local0*/ <- List/*->scala::package.List.*/(1)
    b/*<-local1*//*->local1*/ <- List/*->scala::package.List.*/(1)
    if b/*->local1*/ >/*->scala::Int#`>`(+3).*/ 1
    c/*<-local2*//*->local2*/ = a/*->local0*/ +/*->scala::Int#`+`(+4).*/ b/*->local1*/
  } yield (a/*->local0*/, b/*->local1*/, c/*->local2*/)
  for {
    a/*<-local4*/ <- List/*->scala::package.List.*/(1)
    b/*<-local5*/ <- List/*->scala::package.List.*/(a/*->local4*/)
    if (
      a/*->local4*/,
      b/*->local5*/
    ) ==/*->scala::Any#`==`().*/ (1, 2)
    (
      c/*<-local7*/,
      d/*<-local8*/
    ) <- List/*->scala::package.List.*/((a/*->local4*/, b/*->local5*/))
    if (
      a/*->local4*/,
      b/*->local5*/,
      c/*->local7*/,
      d/*->local8*/
    ) ==/*->scala::Any#`==`().*/ (1, 2, 3, 4)
    e/*<-local9*//*->local9*/ = (
      a/*->local4*/,
      b/*->local5*/,
      c/*->local7*/,
      d/*->local8*/
    )
    if e/*->local9*/ ==/*->scala::Any#`==`().*/ (1, 2, 3, 4)
    f/*<-local10*/ <- List/*->scala::package.List.*/(e/*->local9*/)
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
