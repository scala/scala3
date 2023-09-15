package ext

extension (s/*<-ext::Extension$package.foo().(s)*//*<-ext::Extension$package.`#*#`().(s)*/: String/*->scala::Predef.String#*/)
  def foo/*<-ext::Extension$package.foo().*/: Int/*->scala::Int#*/ = 42
  def #*#/*<-ext::Extension$package.`#*#`().*/ (i/*<-ext::Extension$package.`#*#`().(i)*/: Int/*->scala::Int#*/): (String/*->scala::Predef.String#*/, Int/*->scala::Int#*/) = (s/*->ext::Extension$package.`#*#`().(s)*/, i/*->ext::Extension$package.`#*#`().(i)*/)

val a/*<-ext::Extension$package.a.*/ = "asd".foo/*->ext::Extension$package.foo().*/

val c/*<-ext::Extension$package.c.*/ = "foo" #*#/*->ext::Extension$package.`#*#`().*/ 23

trait Read/*<-ext::Read#*/[+T/*<-ext::Read#[T]*/]:
  def fromString/*<-ext::Read#fromString().*/(s/*<-ext::Read#fromString().(s)*/: String/*->scala::Predef.String#*/): Option/*->scala::Option#*/[T/*->ext::Read#[T]*/]

extension (s/*<-ext::Extension$package.readInto().(s)*/: String/*->scala::Predef.String#*/)
  def readInto/*<-ext::Extension$package.readInto().*/[T/*<-ext::Extension$package.readInto().[T]*/](using Read/*->ext::Read#*/[T/*->ext::Extension$package.readInto().[T]*/]): Option/*->scala::Option#*/[T/*->ext::Extension$package.readInto().[T]*/] = summon/*->scala::Predef.summon().*/[Read/*->ext::Read#*/[T/*->ext::Extension$package.readInto().[T]*/]].fromString/*->ext::Read#fromString().*/(s/*->ext::Extension$package.readInto().(s)*/)

trait Functor/*<-ext::Functor#*/[F/*<-ext::Functor#[F]*/[_]]:
  extension [T/*<-ext::Functor#map().[T]*/](t/*<-ext::Functor#map().(t)*/: F/*->ext::Functor#[F]*/[T/*->ext::Functor#map().[T]*/]) def map/*<-ext::Functor#map().*/[U/*<-ext::Functor#map().[U]*/](f/*<-ext::Functor#map().(f)*/: T/*->ext::Functor#map().[T]*/ => U/*->ext::Functor#map().[U]*/): F/*->ext::Functor#[F]*/[U/*->ext::Functor#map().[U]*/]

opaque type Deck/*<-ext::Extension$package.Deck#*/ = Long/*->scala::Long#*/
object Deck/*<-ext::Extension$package.Deck.*/:
  extension (data/*<-ext::Extension$package.Deck.fooSize().(data)*/: Deck/*->ext::Extension$package.Deck#*/)
    def fooSize/*<-ext::Extension$package.Deck.fooSize().*/: Int/*->scala::Int#*/ = ???/*->scala::Predef.`???`().*/

object DeckUsage/*<-ext::DeckUsage.*/:
  val deck/*<-ext::DeckUsage.deck.*/: Deck/*->ext::Extension$package.Deck#*/ = ???/*->scala::Predef.`???`().*/
  deck/*->ext::DeckUsage.deck.*/.fooSize/*->ext::Extension$package.Deck.fooSize().*/
