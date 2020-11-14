package example

class ValPattern/*<-example::ValPattern#*/ {

  val (left/*<-example::ValPattern#left.*/, right/*<-example::ValPattern#right.*/) = (/*->scala::Tuple2.apply().*/1, 2)
  val Some/*->scala::Some.*//*->scala::Some.unapply().*/(number1/*<-example::ValPattern#number1.*/) =
    Some/*->scala::Some.*//*->scala::Some.apply().*/(1)

  val List/*->scala::package.List.*//*->scala::collection::SeqFactory#unapplySeq().*/(Some/*->scala::Some.*//*->scala::Some.unapply().*/(q1/*<-example::ValPattern#q1.*/), None/*->scala::None.*/: None/*->scala::None.*/.type, None/*->scala::None.*/) = ???/*->scala::Predef.`???`().*/

  var (leftVar/*<-example::ValPattern#leftVar().*/, rightVar/*<-example::ValPattern#rightVar().*/) = (/*->scala::Tuple2.apply().*/1, 2)
  var Some/*->scala::Some.*//*->scala::Some.unapply().*/(number1Var/*<-example::ValPattern#number1Var().*/) =
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
    locally/*->scala::Predef.locally().*/ {
      val (left/*<-local0*/, right/*<-local1*/) = (/*->scala::Tuple2.apply().*/1, 2)
      val Some/*->scala::Some.*//*->scala::Some.unapply().*/(number1/*<-local2*/) =
        Some/*->scala::Some.*//*->scala::Some.apply().*/(1)

      var (leftVar/*<-local3*/, rightVar/*<-local4*/) = (/*->scala::Tuple2.apply().*/1, 2)
      var Some/*->scala::Some.*//*->scala::Some.unapply().*/(number1Var/*<-local5*/) =
        Some/*->scala::Some.*//*->scala::Some.apply().*/(1)
      println/*->scala::Predef.println(+1).*/(
        (
          /*->scala::Tuple6.apply().*/number1/*->local2*/,
          left/*->local0*/,
          right/*->local1*/,
          number1Var/*->local5*/,
          leftVar/*->local3*/,
          rightVar/*->local4*/
        )
      )
    }
  }

}
