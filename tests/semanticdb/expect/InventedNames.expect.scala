package givens

trait X/*<-givens::X#*/:
  def doX/*<-givens::X#doX().*/: Int/*->scala::Int#*/

trait Y/*<-givens::Y#*/:
  def doY/*<-givens::Y#doY().*/: String/*->scala::Predef.String#*/

trait Z/*<-givens::Z#*/[T/*<-givens::Z#[T]*/]:
  def doZ/*<-givens::Z#doZ().*/: List/*->scala::package.List#*/[T/*->givens::Z#[T]*/]



given intValue/*<-givens::InventedNames$package.intValue.*/: Int/*->scala::Int#*/ = 4
given String/*->scala::Predef.String#*/ = "str"
given (using Int/*->scala::Int#*/): Double/*->scala::Double#*/ = 4.0
given [T/*<-givens::InventedNames$package.given_List_T().[T]*/]: List/*->scala::package.List#*/[T/*->givens::InventedNames$package.given_List_T().[T]*/] = Nil/*->scala::package.Nil.*/
given given_Char/*<-givens::InventedNames$package.given_Char.*/: Char/*->scala::Char#*/ = '?'
given `given_Float/*<-givens::InventedNames$package.given_Float.*/`: Float/*->scala::Float#*/ = 3.0
given `* */*<-givens::InventedNames$package.`* *`.*/`: Long/*->scala::Long#*/ = 5

given X/*->givens::X#*/ with
  def doX/*<-givens::InventedNames$package.given_X.doX().*/ = 7

given (using X/*->givens::X#*/): Y/*->givens::Y#*/ with
  def doY/*<-givens::InventedNames$package.given_Y#doY().*/ = "7"

given [T/*<-givens::InventedNames$package.given_Z_T#[T]*//*<-givens::InventedNames$package.given_Z_T().[T]*/]: Z/*->givens::Z#*/[T/*->givens::InventedNames$package.given_Z_T#[T]*/] with
  def doZ/*<-givens::InventedNames$package.given_Z_T#doZ().*/: List/*->scala::package.List#*/[T/*->givens::InventedNames$package.given_Z_T#[T]*/] = Nil/*->scala::package.Nil.*/



val a/*<-givens::InventedNames$package.a.*/ = intValue/*->givens::InventedNames$package.intValue.*/
val b/*<-givens::InventedNames$package.b.*/ = given_String/*->givens::InventedNames$package.given_String.*/
val c/*<-givens::InventedNames$package.c.*/ = given_Double/*->givens::InventedNames$package.given_Double().*/
val d/*<-givens::InventedNames$package.d.*/ = given_List_T/*->givens::InventedNames$package.given_List_T().*/[Int/*->scala::Int#*/]
val e/*<-givens::InventedNames$package.e.*/ = given_Char/*->givens::InventedNames$package.given_Char.*/
val f/*<-givens::InventedNames$package.f.*/ = given_Float/*->givens::InventedNames$package.given_Float.*/
val g/*<-givens::InventedNames$package.g.*/ = `* *`/*->givens::InventedNames$package.`* *`.*/
val x/*<-givens::InventedNames$package.x.*/ = given_X/*->givens::InventedNames$package.given_X.*/
val y/*<-givens::InventedNames$package.y.*/ = given_Y/*->givens::InventedNames$package.given_Y().*/
val z/*<-givens::InventedNames$package.z.*/ = given_Z_T/*->givens::InventedNames$package.given_Z_T().*/[String/*->scala::Predef.String#*/]