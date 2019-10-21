object Enums/*<<=Enums.*/ {
  enum Colour/*<<=Enums.Colour#*/ {
    case /*=>>scala.runtime.EnumValues#`<init>`().*/Red/*=>>Enums.Colour.Red.*//*<<=Enums.Colour.Red.*/, Green/*=>>Enums.Colour.Green.*//*<<=Enums.Colour.Green.*/, Blue/*=>>Enums.Colour.Blue.*//*<<=Enums.Colour.Blue.*/
  }

  enum WeekDays/*<<=Enums.WeekDays#*/ {
    /*=>>scala.runtime.EnumValues#`<init>`().*/case Monday/*<<=Enums.WeekDays.Monday.*/
    case Tuesday/*<<=Enums.WeekDays.Tuesday.*/
    case Wednesday/*<<=Enums.WeekDays.Wednesday.*/
    case Thursday/*<<=Enums.WeekDays.Thursday.*/
    case Friday/*<<=Enums.WeekDays.Friday.*/
    case Saturday/*<<=Enums.WeekDays.Saturday.*/
    case Sunday/*<<=Enums.WeekDays.Sunday.*/
  }

  enum Maybe/*<<=Enums.Maybe#*/[+A/*<<=Enums.Maybe#[A]*/] {
    /*=>>Enums.Maybe#`<init>`().*//*=>>Enums.Maybe.Just#[A]*/case Just/*<<=Enums.Maybe.Just#*/(value/*<<=Enums.Maybe.Just#value.*/: A/*=>>Enums.Maybe.Just#`<init>`().[A]*/)
    /*=>>Enums.Maybe#`<init>`().*/case None/*<<=Enums.Maybe.None.*/
  }

  enum Tag/*<<=Enums.Tag#*/[A/*<<=Enums.Tag#[A]*/] {
    case IntTag/*<<=Enums.Tag.IntTag.*/ extends Tag/*=>>Enums.Tag#*/[Int/*=>>scala.Int#*/]/*=>>Enums.Tag#`<init>`().*/
    case BooleanTag/*<<=Enums.Tag.BooleanTag.*/ extends Tag/*=>>Enums.Tag#*/[Boolean/*=>>scala.Boolean#*/]/*=>>Enums.Tag#`<init>`().*/
  }

  enum =:=/*<<=Enums.`=:=`#*/[A/*<<=Enums.`=:=`#[A]*/, B/*<<=Enums.`=:=`#[B]*/] {
    case Refl/*<<=Enums.`=:=`.Refl#*/[C/*<<=Enums.`=:=`.Refl#[C]*/]() extends (C/*=>>Enums.`=:=`.Refl#[C]*/ =:=/*=>>Enums.`=:=`#*/ C/*=>>Enums.`=:=`.Refl#[C]*//*=>>Enums.`=:=`#`<init>`().*/)
  }

  def unwrap/*<<=Enums.unwrap().*/[A/*<<=Enums.unwrap().[A]*/,B/*<<=Enums.unwrap().[B]*/](opt/*<<=Enums.unwrap().(opt)*/: Option/*=>>scala.Option#*/[A/*=>>Enums.unwrap().[A]*/])(given ev/*<<=Enums.unwrap().(ev)*/: A/*=>>Enums.unwrap().[A]*/ =:=/*=>>Enums.`=:=`#*/ Option/*=>>scala.Option#*/[B/*=>>Enums.unwrap().[B]*/]): Option/*=>>scala.Option#*/[B/*=>>Enums.unwrap().[B]*/] = {
    ev/*=>>Enums.unwrap().(ev)*/ match {
      case =:=/*=>>Enums.`=:=`.*/.Refl/*=>>Enums.`=:=`.Refl.*/() => opt/*=>>Enums.unwrap().(opt)*/.flatMap/*=>>scala.Option#flatMap().*/(identity/*=>>scala.Predef.identity().*/[Option/*=>>scala.Option#*/[B/*=>>Enums.unwrap().[B]*/]])
    }
  }

  enum Planet/*<<=Enums.Planet#*/(mass/*<<=Enums.Planet#mass.*/: Double/*=>>scala.Double#*/, radius/*<<=Enums.Planet#radius.*/: Double/*=>>scala.Double#*/) extends java/*=>>java.*/.lang/*=>>java.lang.*/.Enum/*=>>java.lang.Enum#*/[Planet/*=>>Enums.Planet#*/]/*=>>java.lang.Enum#`<init>`().*/ {
    private final val G/*<<=Enums.Planet#G.*/ = 6.67300E-11
    def surfaceGravity/*<<=Enums.Planet#surfaceGravity().*/ = G/*=>>Enums.Planet#G.*/ */*=>>scala.Double#`*`(+6).*/ mass/*=>>Enums.Planet#mass.*/ //*=>>scala.Double#`.`(+6).*/ (radius/*=>>Enums.Planet#radius.*/ */*=>>scala.Double#`*`(+6).*/ radius/*=>>Enums.Planet#radius.*/)
    def surfaceWeight/*<<=Enums.Planet#surfaceWeight().*/(otherMass/*<<=Enums.Planet#surfaceWeight().(otherMass)*/: Double/*=>>scala.Double#*/) = otherMass/*=>>Enums.Planet#surfaceWeight().(otherMass)*/ */*=>>scala.Double#`*`(+6).*/ surfaceGravity/*=>>Enums.Planet#surfaceGravity().*/

    case Mercury/*<<=Enums.Planet.Mercury.*/ extends /*=>>scala.runtime.EnumValues#`<init>`().*/Planet/*=>>Enums.Planet#*//*=>>Enums.Planet#`<init>`().*/(3.303e+23, 2.4397e6)/*=>>scala.runtime.EnumValues#register().*/
    case Venus/*<<=Enums.Planet.Venus.*/   extends Planet/*=>>Enums.Planet#*//*=>>Enums.Planet#`<init>`().*/(4.869e+24, 6.0518e6)/*=>>scala.runtime.EnumValues#register().*/
    case Earth/*<<=Enums.Planet.Earth.*/   extends Planet/*=>>Enums.Planet#*//*=>>Enums.Planet#`<init>`().*/(5.976e+24, 6.37814e6)/*=>>scala.runtime.EnumValues#register().*/
    case Mars/*<<=Enums.Planet.Mars.*/    extends Planet/*=>>Enums.Planet#*//*=>>Enums.Planet#`<init>`().*/(6.421e+23, 3.3972e6)/*=>>scala.runtime.EnumValues#register().*/
    case Jupiter/*<<=Enums.Planet.Jupiter.*/ extends Planet/*=>>Enums.Planet#*//*=>>Enums.Planet#`<init>`().*/(1.9e+27,   7.1492e7)/*=>>scala.runtime.EnumValues#register().*/
    case Saturn/*<<=Enums.Planet.Saturn.*/  extends Planet/*=>>Enums.Planet#*//*=>>Enums.Planet#`<init>`().*/(5.688e+26, 6.0268e7)/*=>>scala.runtime.EnumValues#register().*/
    case Uranus/*<<=Enums.Planet.Uranus.*/  extends Planet/*=>>Enums.Planet#*//*=>>Enums.Planet#`<init>`().*/(8.686e+25, 2.5559e7)/*=>>scala.runtime.EnumValues#register().*/
    case Neptune/*<<=Enums.Planet.Neptune.*/ extends Planet/*=>>Enums.Planet#*//*=>>Enums.Planet#`<init>`().*/(1.024e+26, 2.4746e7)/*=>>scala.runtime.EnumValues#register().*/
  }
}
