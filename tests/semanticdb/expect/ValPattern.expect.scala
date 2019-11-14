package example

class ValPattern/*<-example::ValPattern#*/ {

  /*->scala::Tuple2#_1.*//*->scala::Tuple2#_2.*/val (left/*<-example::ValPattern#left.*/, right/*<-example::ValPattern#right.*/) = (1, 2)
  val Some/*->scala::Some.*/(number1/*<-example::ValPattern#number1.*//*<-local0*/)/*->local0*/ =
    Some/*->scala::Some.*/(1)

  /*->scala::Tuple2#_1.*//*->scala::Tuple2#_2.*/var (leftVar/*<-example::ValPattern#leftVar().*/, /*<-example::ValPattern#`leftVar_=`().*/rightVar/*<-example::ValPattern#rightVar().*/) /*<-example::ValPattern#`rightVar_=`().*/= (1, 2)
  var Some/*->scala::Some.*/(number1Var/*<-example::ValPattern#number1Var().*//*<-local1*/) /*<-example::ValPattern#`number1Var_=`().*//*->local1*/=
    Some/*->scala::Some.*/(1)

  def app/*<-example::ValPattern#app().*/(): Unit/*->scala::Unit#*/ = {
    println/*->scala::Predef.println(+1).*/(
      (
        number1/*->example::ValPattern#number1.*/,
        left/*->example::ValPattern#left.*/,
        right/*->example::ValPattern#right.*/,
        number1Var/*->example::ValPattern#number1Var().*/,
        leftVar/*->example::ValPattern#leftVar().*/,
        rightVar/*->example::ValPattern#rightVar().*/
      )
    )
    locally/*->dotty::DottyPredef.locally().*/ {
      /*->scala::Tuple2#_1.*//*->scala::Tuple2#_2.*/val (left/*<-local2*/, right/*<-local3*/) = (1, 2)
      val Some/*->scala::Some.*/(number1/*<-local4*/)/*->local4*/ =
        Some/*->scala::Some.*/(1)

      /*->scala::Tuple2#_1.*//*->scala::Tuple2#_2.*/var (leftVar/*<-local5*/, rightVar/*<-local6*/) = (1, 2)
      var Some/*->scala::Some.*/(number1Var/*<-local7*/)/*->local7*/ =
        Some/*->scala::Some.*/(1)
      println/*->scala::Predef.println(+1).*/(
        (
          number1/*->local4*/,
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
