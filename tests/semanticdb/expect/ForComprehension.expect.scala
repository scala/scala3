package example

class ForComprehension/*<-example::ForComprehension#*/ {
  for {
    a/*<-local0*/ <- List/*->scala::package.List.*/(1)
    b/*<-local1*/ <- List/*->scala::package.List.*/(1)
    if b/*->local1*/ >/*->scala::Int#`>`(+3).*/ 1
    c/*<-local2*/ = a/*->local0*/ +/*->scala::Int#`+`(+4).*/ b/*->local1*/
  } yield (a/*->local0*/, b/*->local1*/, c/*->local2*/)
  for {
    a/*<-local3*/ <- List/*->scala::package.List.*/(1)
    b/*<-local4*/ <- List/*->scala::package.List.*/(a/*->local3*/)
    if (
      a/*->local3*/,
      b/*->local4*/
    ) ==/*->scala::Any#`==`().*/ (1, 2)
    (
      c/*<-local6*/,
      d/*<-local7*/
    ) <- List/*->scala::package.List.*/((a/*->local3*/, b/*->local4*/))
    if (
      a/*->local3*/,
      b/*->local4*/,
      c/*->local6*/,
      d/*->local7*/
    ) ==/*->scala::Any#`==`().*/ (1, 2, 3, 4)
    e/*<-local8*//*->local8*/ = (
      a/*->local3*/,
      b/*->local4*/,
      c/*->local6*/,
      d/*->local7*/
    )
    if e/*->local8*/ ==/*->scala::Any#`==`().*/ (1, 2, 3, 4)
    f/*<-local9*/ <- List/*->scala::package.List.*/(e/*->local8*/)
  } yield {
    (
      a/*->local3*/,
      b/*->local4*/,
      c/*->local6*/,
      d/*->local7*/,
      e/*->local8*/,
      f/*->local9*/
    )
  }
}
