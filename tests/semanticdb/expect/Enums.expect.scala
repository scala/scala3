object Enums/*<-_empty_::Enums.*/ with
  import =:=/*->_empty_::Enums.`=:=`.*/._

  enum Colour/*<-_empty_::Enums.Colour#*/ with
    case Red/*->_empty_::Enums.Colour.Red.*//*->scala::runtime::EnumValues#`<init>`().*//*<-_empty_::Enums.Colour.Red.*/, Green/*->_empty_::Enums.Colour.Green.*//*<-_empty_::Enums.Colour.Green.*/, Blue/*->_empty_::Enums.Colour.Blue.*//*<-_empty_::Enums.Colour.Blue.*/

  enum WeekDays/*<-_empty_::Enums.WeekDays#*/ with
    /*->scala::runtime::EnumValues#`<init>`().*/case Monday/*<-_empty_::Enums.WeekDays.Monday.*/
    case Tuesday/*<-_empty_::Enums.WeekDays.Tuesday.*/
    case Wednesday/*<-_empty_::Enums.WeekDays.Wednesday.*/
    case Thursday/*<-_empty_::Enums.WeekDays.Thursday.*/
    case Friday/*<-_empty_::Enums.WeekDays.Friday.*/
    case Saturday/*<-_empty_::Enums.WeekDays.Saturday.*/
    case Sunday/*<-_empty_::Enums.WeekDays.Sunday.*/

  enum Maybe/*<-_empty_::Enums.Maybe#*/[+A/*<-_empty_::Enums.Maybe#[A]*/] with
    case Just/*<-_empty_::Enums.Maybe.Just#*/(value/*<-_empty_::Enums.Maybe.Just#value.*/: A/*->_empty_::Enums.Maybe.Just#[A]*/)
    case None/*<-_empty_::Enums.Maybe.None.*/

  enum Tag/*<-_empty_::Enums.Tag#*/[A/*<-_empty_::Enums.Tag#[A]*/] with
    case IntTag/*<-_empty_::Enums.Tag.IntTag.*/ extends Tag/*->_empty_::Enums.Tag#*/[Int/*->scala::Int#*/]
    case BooleanTag/*<-_empty_::Enums.Tag.BooleanTag.*/ extends Tag/*->_empty_::Enums.Tag#*/[Boolean/*->scala::Boolean#*/]

  enum =:=/*<-_empty_::Enums.`=:=`#*/[A/*<-_empty_::Enums.`=:=`#[A]*/, B/*<-_empty_::Enums.`=:=`#[B]*/] with
    case Refl/*<-_empty_::Enums.`=:=`.Refl#*/[C/*<-_empty_::Enums.`=:=`.Refl#[C]*/]() extends (C/*->_empty_::Enums.`=:=`.Refl#[C]*/ =:=/*->_empty_::Enums.`=:=`#*/ C/*->_empty_::Enums.`=:=`.Refl#[C]*/)

  def unwrap/*<-_empty_::Enums.unwrap().*/[A/*<-_empty_::Enums.unwrap().[A]*/,B/*<-_empty_::Enums.unwrap().[B]*/](opt/*<-_empty_::Enums.unwrap().(opt)*/: Option/*->scala::Option#*/[A/*->_empty_::Enums.unwrap().[A]*/])(given ev/*<-_empty_::Enums.unwrap().(ev)*/: A/*->_empty_::Enums.unwrap().[A]*/ =:=/*->_empty_::Enums.`=:=`#*/ Option/*->scala::Option#*/[B/*->_empty_::Enums.unwrap().[B]*/]): Option/*->scala::Option#*/[B/*->_empty_::Enums.unwrap().[B]*/] = ev/*->_empty_::Enums.unwrap().(ev)*/ match
    case Refl/*->_empty_::Enums.`=:=`.Refl.*/() => opt/*->_empty_::Enums.unwrap().(opt)*/.flatMap/*->scala::Option#flatMap().*/(identity/*->scala::Predef.identity().*/[Option/*->scala::Option#*/[B/*->_empty_::Enums.unwrap().[B]*/]])
    case _      => None/*->scala::None.*/ // TODO remove after https://github.com/lampepfl/dotty/issues/7524 is fixed

  enum Planet/*<-_empty_::Enums.Planet#*/(mass/*<-_empty_::Enums.Planet#mass.*/: Double/*->scala::Double#*/, radius/*<-_empty_::Enums.Planet#radius.*/: Double/*->scala::Double#*/) extends java.lang.Enum/*->java::lang::Enum#*/[Planet/*->_empty_::Enums.Planet#*/]/*->java::lang::Enum#`<init>`().*/ with
    private final val G/*<-_empty_::Enums.Planet#G.*/ = 6.67300E-11
    def surfaceGravity/*<-_empty_::Enums.Planet#surfaceGravity().*/ = G/*->_empty_::Enums.Planet#G.*/ */*->scala::Double#`*`(+6).*/ mass/*->_empty_::Enums.Planet#mass.*/ //*->scala::Double#`::`(+6).*/ (radius/*->_empty_::Enums.Planet#radius.*/ */*->scala::Double#`*`(+6).*/ radius/*->_empty_::Enums.Planet#radius.*/)
    def surfaceWeight/*<-_empty_::Enums.Planet#surfaceWeight().*/(otherMass/*<-_empty_::Enums.Planet#surfaceWeight().(otherMass)*/: Double/*->scala::Double#*/) = otherMass/*->_empty_::Enums.Planet#surfaceWeight().(otherMass)*/ */*->scala::Double#`*`(+6).*/ surfaceGravity/*->_empty_::Enums.Planet#surfaceGravity().*/

    case Mercury/*<-_empty_::Enums.Planet.Mercury.*/ extends /*->scala::runtime::EnumValues#`<init>`().*/Planet/*->_empty_::Enums.Planet#*/(3.303e+23, 2.4397e6)/*->scala::runtime::EnumValues#register().*/
    case Venus/*<-_empty_::Enums.Planet.Venus.*/   extends Planet/*->_empty_::Enums.Planet#*/(4.869e+24, 6.0518e6)/*->scala::runtime::EnumValues#register().*/
    case Earth/*<-_empty_::Enums.Planet.Earth.*/   extends Planet/*->_empty_::Enums.Planet#*/(5.976e+24, 6.37814e6)/*->scala::runtime::EnumValues#register().*/
    case Mars/*<-_empty_::Enums.Planet.Mars.*/    extends Planet/*->_empty_::Enums.Planet#*/(6.421e+23, 3.3972e6)/*->scala::runtime::EnumValues#register().*/
    case Jupiter/*<-_empty_::Enums.Planet.Jupiter.*/ extends Planet/*->_empty_::Enums.Planet#*/(1.9e+27,   7.1492e7)/*->scala::runtime::EnumValues#register().*/
    case Saturn/*<-_empty_::Enums.Planet.Saturn.*/  extends Planet/*->_empty_::Enums.Planet#*/(5.688e+26, 6.0268e7)/*->scala::runtime::EnumValues#register().*/
    case Uranus/*<-_empty_::Enums.Planet.Uranus.*/  extends Planet/*->_empty_::Enums.Planet#*/(8.686e+25, 2.5559e7)/*->scala::runtime::EnumValues#register().*/
    case Neptune/*<-_empty_::Enums.Planet.Neptune.*/ extends Planet/*->_empty_::Enums.Planet#*/(1.024e+26, 2.4746e7)/*->scala::runtime::EnumValues#register().*/
