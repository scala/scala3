package prefixes

class C/*<-prefixes::C#*/ {
  type T/*<-prefixes::C#T#*/
  def m1/*<-prefixes::C#m1().*/: T/*->prefixes::C#T#*/ = ???/*->scala::Predef.`???`().*/

  object N/*<-prefixes::C#N.*/ {
    type U/*<-prefixes::C#N.U#*/
  }
  def k1/*<-prefixes::C#k1().*/: N/*->prefixes::C#N.*/.U/*->prefixes::C#N.U#*/ = ???/*->scala::Predef.`???`().*/
}

object M/*<-prefixes::M.*/ {
  type T/*<-prefixes::M.T#*/
  def n1/*<-prefixes::M.n1().*/: T/*->prefixes::M.T#*/ = ???/*->scala::Predef.`???`().*/
}

object O/*<-prefixes::O.*/ extends C/*->prefixes::C#*/ {
  def o1/*<-prefixes::O.o1().*/: T/*->prefixes::C#T#*/ = ???/*->scala::Predef.`???`().*/
}

object Test/*<-prefixes::Test.*/ {
  val c/*<-prefixes::Test.c.*/: C/*->prefixes::C#*/ = ???/*->scala::Predef.`???`().*/
  def m2/*<-prefixes::Test.m2().*/: c/*->prefixes::Test.c.*/.T/*->prefixes::C#T#*/ = ???/*->scala::Predef.`???`().*/
  def k2/*<-prefixes::Test.k2().*/: c/*->prefixes::Test.c.*/.N/*->prefixes::C#N.*/.U/*->prefixes::C#N.U#*/ = ???/*->scala::Predef.`???`().*/
  import c/*->prefixes::Test.c.*/.N/*->prefixes::C#N.*/.*
  def k3/*<-prefixes::Test.k3().*/: U/*->prefixes::C#N.U#*/ = ???/*->scala::Predef.`???`().*/

  def n2/*<-prefixes::Test.n2().*/: M/*->prefixes::M.*/.T/*->prefixes::M.T#*/ = ???/*->scala::Predef.`???`().*/

  import M/*->prefixes::M.*/.*
  def n3/*<-prefixes::Test.n3().*/: T/*->prefixes::M.T#*/ = ???/*->scala::Predef.`???`().*/
}
