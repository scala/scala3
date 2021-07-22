package example

import com.javacp

class MetacJava/*<-example::MetacJava#*/ {
  javacp.MetacJava/*->com::javacp::MetacJava#*/.StaticInner/*->com::javacp::MetacJava#StaticInner#*/.isStatic/*->com::javacp::MetacJava#StaticInner#isStatic().*/()
  new javacp.MetacJava/*->com::javacp::MetacJava#*/.StaticInner/*->com::javacp::MetacJava#StaticInner#*/().isNotStatic/*->com::javacp::MetacJava#StaticInner#isNotStatic().*/()
  val inner/*<-example::MetacJava#inner.*/ = new javacp.MetacJava/*->com::javacp::MetacJava#*/()
  val overload1/*<-example::MetacJava#overload1.*/ = new inner/*->example::MetacJava#inner.*/.Overload1/*->com::javacp::MetacJava#Overload1#*/()
  val overload2/*<-example::MetacJava#overload2.*/ = new inner/*->example::MetacJava#inner.*/.Overload2/*->com::javacp::MetacJava#Overload2#*/()
  inner/*->example::MetacJava#inner.*/.overload/*->com::javacp::MetacJava#overload().*/(new overload1/*->example::MetacJava#overload1.*/.A/*->com::javacp::MetacJava#Overload1#A#*/())
  inner/*->example::MetacJava#inner.*/.overload/*->com::javacp::MetacJava#overload(+1).*/(new overload2/*->example::MetacJava#overload2.*/.A/*->com::javacp::MetacJava#Overload2#A#*/())
  val staticInner/*<-example::MetacJava#staticInner.*/ = new javacp.MetacJava/*->com::javacp::MetacJava#*/.StaticInner/*->com::javacp::MetacJava#StaticInner#*/()
  val nonStatic/*<-example::MetacJava#nonStatic.*/ = new staticInner/*->example::MetacJava#staticInner.*/.NonStatic/*->com::javacp::MetacJava#StaticInner#NonStatic#*/()
  nonStatic/*->example::MetacJava#nonStatic.*/.method/*->com::javacp::MetacJava#StaticInner#NonStatic#method().*/(nonStatic/*->example::MetacJava#nonStatic.*/)
  javacp.MetacJava/*->com::javacp::MetacJava#*/.overload/*->com::javacp::MetacJava#overload(+2).*/(new javacp.MetacJava/*->com::javacp::MetacJava#*/.Overload3/*->com::javacp::MetacJava#Overload3#*/.A/*->com::javacp::MetacJava#Overload3#A#*/())
  val interface/*<-example::MetacJava#interface.*/: javacp.Interface/*->com::javacp::Interface#*/ = null
  val coin/*<-example::MetacJava#coin.*/: javacp.Coin/*->com::javacp::Coin#*/ = javacp.Coin/*->com::javacp::Coin#*/.PENNY/*->com::javacp::Coin#PENNY.*/
  val entry/*<-example::MetacJava#entry.*/: java.util.Map/*->java::util::Map#*/.Entry/*->java::util::Map#Entry#*/[Int/*->scala::Int#*/, Int/*->scala::Int#*/] = null
}
