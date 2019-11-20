package example

class ValPattern/*<-example::ValPattern#*/ {

  val (left/*<-example::ValPattern#left.*/, right/*<-example::ValPattern#right.*/) = (/*->scala::Tuple2.apply().*/1, 2)
  val Some/*->scala::Some.*//*->scala::Some.unapply().*/(number1/*<-example::ValPattern#number1.*//*<-local0*/)/*->local0*/ =
    Some/*->scala::Some.*//*->scala::Some.apply().*/(1)

  var (leftVar/*<-example::ValPattern#leftVar().*/, rightVar/*<-example::ValPattern#rightVar().*/) = (/*->scala::Tuple2.apply().*/1, 2)
  var Some/*->scala::Some.*//*->scala::Some.unapply().*/(number1Var/*<-example::ValPattern#number1Var().*//*<-local1*/)/*->local1*/ =
    Some/*->scala::Some.*//*->scala::Some.apply().*/(1)

  def app/*<-example::ValPattern#app().*/(): Unit/*->scala::Unit#*/ = {
    println/*->scala::Predef.println(+1).*/(
      (
        /*->scala::Tuple6.apply().*/number1/*->example::ValPattern#number1.*/,
        left/*->example::ValPattern#left.*/,
        right/*->example::ValPattern#right.*/,
        number1Var/*->example::ValPattern#number1Var().*/,
        leftVar/*->example::ValPattern#leftVar().*/,
        rightVar/*->example::ValPattern#rightVar().*/
      )
    )
    locally/*->dotty::DottyPredef.locally().*/ {
      val (left/*<-local2*/, right/*<-local3*/) = (/*->scala::Tuple2.apply().*/1, 2)
      val Some/*->scala::Some.*//*->scala::Some.unapply().*/(number1/*<-local4*/)/*->local4*/ =
        Some/*->scala::Some.*//*->scala::Some.apply().*/(1)

      var (leftVar/*<-local5*/, rightVar/*<-local6*/) = (/*->scala::Tuple2.apply().*/1, 2)
      var Some/*->scala::Some.*//*->scala::Some.unapply().*/(number1Var/*<-local7*/)/*->local7*/ =
        Some/*->scala::Some.*//*->scala::Some.apply().*/(1)
      println/*->scala::Predef.println(+1).*/(
        (
          /*->scala::Tuple6.apply().*/number1/*->local4*/,
          left/*->local2*/,
          right/*->local3*/,
          number1Var/*->local7*/,
          leftVar/*->local5*/,
          rightVar/*->local6*/
        )
      )
    }
  }

}
