object Enums/*<-_empty_::Enums.*/:
  import <:</*->_empty_::Enums.`<:<`.*/.*

  enum Colour/*<-_empty_::Enums.Colour#*/:
    import Colour/*->_empty_::Enums.Colour.*/.Red/*->_empty_::Enums.Colour.Red.*/
    case Red/*<-_empty_::Enums.Colour.Red.*/, Green/*<-_empty_::Enums.Colour.Green.*/, Blue/*<-_empty_::Enums.Colour.Blue.*/

  enum Directions/*<-_empty_::Enums.Directions#*/:
    case North/*<-_empty_::Enums.Directions.North.*/, East/*<-_empty_::Enums.Directions.East.*/, South/*<-_empty_::Enums.Directions.South.*/, West/*<-_empty_::Enums.Directions.West.*/

  enum Suits/*<-_empty_::Enums.Suits#*/ derives CanEqual:
    case Hearts/*<-_empty_::Enums.Suits.Hearts.*/, Spades/*<-_empty_::Enums.Suits.Spades.*/, Clubs/*<-_empty_::Enums.Suits.Clubs.*/, Diamonds/*<-_empty_::Enums.Suits.Diamonds.*/

  object Suits/*<-_empty_::Enums.Suits.*/:
    extension (suit/*<-_empty_::Enums.Suits.isRed().(suit)*/: Suits/*->_empty_::Enums.Suits#*/) def isRed/*<-_empty_::Enums.Suits.isRed().*/: Boolean/*->scala::Boolean#*/ =
      suit/*->_empty_::Enums.Suits.isRed().(suit)*/ ==/*->scala::Any#`==`().*/ Hearts/*->_empty_::Enums.Suits.Hearts.*/ ||/*->scala::Boolean#`||`().*/ suit/*->_empty_::Enums.Suits.isRed().(suit)*/ ==/*->scala::Any#`==`().*/ Diamonds/*->_empty_::Enums.Suits.Diamonds.*/

    extension (suit/*<-_empty_::Enums.Suits.isBlack().(suit)*/: Suits/*->_empty_::Enums.Suits#*/) def isBlack/*<-_empty_::Enums.Suits.isBlack().*/: Boolean/*->scala::Boolean#*/ = suit/*->_empty_::Enums.Suits.isBlack().(suit)*/ match
      case Spades/*->_empty_::Enums.Suits.Spades.*/ | Clubs/*->_empty_::Enums.Suits.Clubs.*/ => true
      case _              => false

  enum WeekDays/*<-_empty_::Enums.WeekDays#*/:
    case Monday/*<-_empty_::Enums.WeekDays.Monday.*/
    case Tuesday/*<-_empty_::Enums.WeekDays.Tuesday.*/
    case Wednesday/*<-_empty_::Enums.WeekDays.Wednesday.*/
    case Thursday/*<-_empty_::Enums.WeekDays.Thursday.*/
    case Friday/*<-_empty_::Enums.WeekDays.Friday.*/
    case Saturday/*<-_empty_::Enums.WeekDays.Saturday.*/
    case Sunday/*<-_empty_::Enums.WeekDays.Sunday.*/

  enum Coin/*<-_empty_::Enums.Coin#*/(value/*<-_empty_::Enums.Coin#value.*/: Int/*->scala::Int#*/):
    case Penny/*<-_empty_::Enums.Coin.Penny.*/    extends Coin/*->_empty_::Enums.Coin#*/(1)
    case Nickel/*<-_empty_::Enums.Coin.Nickel.*/   extends Coin/*->_empty_::Enums.Coin#*/(5)
    case Dime/*<-_empty_::Enums.Coin.Dime.*/     extends Coin/*->_empty_::Enums.Coin#*/(10)
    case Quarter/*<-_empty_::Enums.Coin.Quarter.*/  extends Coin/*->_empty_::Enums.Coin#*/(25)
    case Dollar/*<-_empty_::Enums.Coin.Dollar.*/   extends Coin/*->_empty_::Enums.Coin#*/(100)

  enum Maybe/*<-_empty_::Enums.Maybe#*/[+A/*<-_empty_::Enums.Maybe#[A]*/]:
    case Just/*<-_empty_::Enums.Maybe.Just#*/(value/*<-_empty_::Enums.Maybe.Just#value.*/: A/*->_empty_::Enums.Maybe.Just#[A]*/)
    case None/*<-_empty_::Enums.Maybe.None.*/

  enum Tag/*<-_empty_::Enums.Tag#*/[A/*<-_empty_::Enums.Tag#[A]*/]:
    case IntTag/*<-_empty_::Enums.Tag.IntTag.*/ extends Tag/*->_empty_::Enums.Tag#*/[Int/*->scala::Int#*/]
    case BooleanTag/*<-_empty_::Enums.Tag.BooleanTag.*/ extends Tag/*->_empty_::Enums.Tag#*/[Boolean/*->scala::Boolean#*/]

  enum <:</*<-_empty_::Enums.`<:<`#*/[-A/*<-_empty_::Enums.`<:<`#[A]*/, B/*<-_empty_::Enums.`<:<`#[B]*/]:
    case Refl/*<-_empty_::Enums.`<:<`.Refl#*/[C/*<-_empty_::Enums.`<:<`.Refl#[C]*/]() extends (C/*->_empty_::Enums.`<:<`.Refl#[C]*/ <:</*->_empty_::Enums.`<:<`#*/ C/*->_empty_::Enums.`<:<`.Refl#[C]*/)

  object <:</*<-_empty_::Enums.`<:<`.*/ :
    given [T/*<-_empty_::Enums.`<:<`.`given_<:<_T_T`().[T]*/]: (T/*->_empty_::Enums.`<:<`.`given_<:<_T_T`().[T]*/ <:</*->_empty_::Enums.`<:<`#*/ T/*->_empty_::Enums.`<:<`.`given_<:<_T_T`().[T]*/) = Refl/*->_empty_::Enums.`<:<`.Refl.*/()

  extension [A/*<-_empty_::Enums.unwrap().[A]*/, B/*<-_empty_::Enums.unwrap().[B]*/](opt/*<-_empty_::Enums.unwrap().(opt)*/: Option/*->scala::Option#*/[A/*->_empty_::Enums.unwrap().[A]*/]) def unwrap/*<-_empty_::Enums.unwrap().*/(using ev/*<-_empty_::Enums.unwrap().(ev)*/: A/*->_empty_::Enums.unwrap().[A]*/ <:</*->_empty_::Enums.`<:<`#*/ Option/*->scala::Option#*/[B/*->_empty_::Enums.unwrap().[B]*/]): Option/*->scala::Option#*/[B/*->_empty_::Enums.unwrap().[B]*/] = ev/*->_empty_::Enums.unwrap().(ev)*/ match
    case Refl/*->_empty_::Enums.`<:<`.Refl.*/() => opt/*->_empty_::Enums.unwrap().(opt)*/.flatMap/*->scala::Option#flatMap().*/(identity/*->scala::Predef.identity().*/[Option/*->scala::Option#*/[B/*->_empty_::Enums.unwrap().[B]*/]])

  val some1/*<-_empty_::Enums.some1.*/ = Some/*->scala::Some.*/(Some/*->scala::Some.*/(1)).unwrap/*->_empty_::Enums.unwrap().*/

  enum Planet/*<-_empty_::Enums.Planet#*/(mass/*<-_empty_::Enums.Planet#mass.*/: Double/*->scala::Double#*/, radius/*<-_empty_::Enums.Planet#radius.*/: Double/*->scala::Double#*/) extends Enum/*->java::lang::Enum#*/[Planet/*->_empty_::Enums.Planet#*/]:
    private final val G/*<-_empty_::Enums.Planet#G.*/ = 6.67300E-11
    def surfaceGravity/*<-_empty_::Enums.Planet#surfaceGravity().*/ = G/*->_empty_::Enums.Planet#G.*/ */*->scala::Double#`*`(+6).*/ mass/*->_empty_::Enums.Planet#mass.*/ //*->scala::Double#`::`(+6).*/ (radius/*->_empty_::Enums.Planet#radius.*/ */*->scala::Double#`*`(+6).*/ radius/*->_empty_::Enums.Planet#radius.*/)
    def surfaceWeight/*<-_empty_::Enums.Planet#surfaceWeight().*/(otherMass/*<-_empty_::Enums.Planet#surfaceWeight().(otherMass)*/: Double/*->scala::Double#*/) = otherMass/*->_empty_::Enums.Planet#surfaceWeight().(otherMass)*/ */*->scala::Double#`*`(+6).*/ surfaceGravity/*->_empty_::Enums.Planet#surfaceGravity().*/

    case Mercury/*<-_empty_::Enums.Planet.Mercury.*/ extends Planet/*->_empty_::Enums.Planet#*/(3.303e+23, 2.4397e6)
    case Venus/*<-_empty_::Enums.Planet.Venus.*/   extends Planet/*->_empty_::Enums.Planet#*/(4.869e+24, 6.0518e6)
    case Earth/*<-_empty_::Enums.Planet.Earth.*/   extends Planet/*->_empty_::Enums.Planet#*/(5.976e+24, 6.37814e6)
    case Mars/*<-_empty_::Enums.Planet.Mars.*/    extends Planet/*->_empty_::Enums.Planet#*/(6.421e+23, 3.3972e6)
    case Jupiter/*<-_empty_::Enums.Planet.Jupiter.*/ extends Planet/*->_empty_::Enums.Planet#*/(1.9e+27,   7.1492e7)
    case Saturn/*<-_empty_::Enums.Planet.Saturn.*/  extends Planet/*->_empty_::Enums.Planet#*/(5.688e+26, 6.0268e7)
    case Uranus/*<-_empty_::Enums.Planet.Uranus.*/  extends Planet/*->_empty_::Enums.Planet#*/(8.686e+25, 2.5559e7)
    case Neptune/*<-_empty_::Enums.Planet.Neptune.*/ extends Planet/*->_empty_::Enums.Planet#*/(1.024e+26, 2.4746e7)
