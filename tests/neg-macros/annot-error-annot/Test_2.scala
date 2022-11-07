@error
val vGlobal: Int = 1 // error
@error
lazy val lvGlobal: Int = 1 // error
@error
def dGlobal: Int = 1 // error
@error
given gGlobal: Int = 1 // error
@error
given gGlobal2: Num[Int] with // error: object not supported (TODO support)
  def zero = 0
@error
given gGlobal3(using DummyImplicit): Num[Int] with // error: class not supported (TODO support)
  def zero = 0

class B:
  @error
  val vMember: Int = 1 // error
  @error
  lazy val lvMember: Int = 1 // error
  @error
  def dMember: Int = 1 // error
  @error
  given gMember: Int = 1 // error
  @error
  given gMember2: Num[Int] with // error: object not supported (TODO support)
    def zero = 0
  @error
  given gMember3(using DummyImplicit): Num[Int] with // error: class not supported (TODO support)
    def zero = 0

  def locals: Unit =
    @error
    val vLocal: Int = 1 // error
    @error
    lazy val lvLocal: Int = 1 // error
    @error
    def dLocal: Int = 1 // error
    @error
    given gLocal: Int = 1 // error
    @error
    given gLocal2: Num[Int] with // error: object not supported (TODO support)
      def zero = 0
    @error
    given gLocal3(using DummyImplicit): Num[Int] with // error: class not supported (TODO support)
      def zero = 0
    ()

trait Num[T]:
  def zero: T
