object /*=>>java.lang.Object#`<init>`().*/Enums/*<<=Enums.*/ {
  /*=>>scala.package.Serializable#*//*=>>scala.*//*=>>_root_*//*=>>Enums.*/enum /*=>>java.lang.Object#`<init>`().*/Colour/*<<=Enums.Colour#*/ {
    /*=>>java.lang.Object#`<init>`().*//*=>>scala.package.Serializable#*//*=>>scala.*//*=>>_root_*/case /*=>>scala.collection.IterableOnceOps#toArray().*//*=>>scala.runtime.EnumValues#values().*//*=>>Enums.Colour.$values.*//*=>>scala.reflect.ClassTag.apply().*//*=>>scala.reflect.ClassTag.*//*=>>scala.runtime.EnumValues#`<init>`().*//*=>>scala.collection.MapOps#apply().*//*=>>scala.runtime.EnumValues#fromName().*//*=>>Enums.Colour.valueOf().($name)*//*=>>`<special-ops>`.throw().*//*=>>java.lang.IllegalArgumentException#`<init>`(+1).*//*=>>java.lang.String#concat().*//*=>>Enums.Colour#`<init>`().*//*=>>Enums.Colour.$new().(_$ordinal)*//*=>>Enums.Colour.$new().($name)*//*=>>scala.runtime.EnumValues#register().*/Red/*=>>Enums.Colour.Red.*//*<<=Enums.Colour.Red.*/, Gr/*<<=Enums.Colour.$values.*/een/*=>>Enums.Colour.Green.*//*<<=Enums.Colour.Green.*/, Blue/*=>>Enums.Colour.Blue.*//*<<=Enums.Colour.Blue.*/
  }

  enum /*=>>java.lang.Object#`<init>`().*/WeekDays/*<<=Enums.WeekDays#*/ {
    /*=>>Enums.WeekDays.Monday.*//*=>>Enums.WeekDays.Tuesday.*//*=>>Enums.WeekDays.Wednesday.*//*=>>Enums.WeekDays.Thursday.*//*=>>Enums.WeekDays.Friday.*//*=>>Enums.WeekDays.Saturday.*//*=>>Enums.WeekDays.Sunday.*//*=>>java.lang.Object#`<init>`().*//*=>>scala.package.Serializable#*//*=>>scala.*//*=>>_root_*//*=>>scala.collection.IterableOnceOps#toArray().*//*=>>scala.runtime.EnumValues#values().*//*=>>Enums.WeekDays.$values.*//*=>>scala.reflect.ClassTag.apply().*//*=>>scala.reflect.ClassTag.*//*=>>scala.runtime.EnumValues#`<init>`().*//*=>>scala.collection.MapOps#apply().*//*=>>scala.runtime.EnumValues#fromName().*//*=>>Enums.WeekDays.valueOf().($name)*//*=>>`<special-ops>`.throw().*//*=>>java.lang.IllegalArgumentException#`<init>`(+1).*//*=>>java.lang.String#concat().*//*=>>Enums.WeekDays#`<init>`().*//*=>>Enums.WeekDays.$new().(_$ordinal)*//*=>>Enums.WeekDays.$new().($name)*//*=>>scala.runtime.EnumValues#register().*/case Monday/*<<=Enums.WeekDays.Monday.*/
/*<<=Enums.WeekDays.$values.*/    case Tuesday/*<<=Enums.WeekDays.Tuesday.*/
    case Wednesday/*<<=Enums.WeekDays.Wednesday.*/
    case Thursday/*<<=Enums.WeekDays.Thursday.*/
    case Friday/*<<=Enums.WeekDays.Friday.*/
    case Saturday/*<<=Enums.WeekDays.Saturday.*/
    case Sunday/*<<=Enums.WeekDays.Sunday.*/
  }

  enum /*=>>java.lang.Object#`<init>`().*/Maybe/*<<=Enums.Maybe#*/[+A] {/*<<=Enums.Maybe#`<init>`().*//*<<=Enums.Maybe#`<init>`().[A]*//*<<=Enums.Maybe#[A]*//*=>>Enums.Maybe.None.*/
    /*=>>java.lang.Object#`<init>`().*//*=>>scala.package.Serializable#*//*=>>scala.*//*=>>_root_*//*=>>Enums.Maybe#`<init>`().*//*=>>Enums.Maybe.Just#[A]*//*=>>Enums.Maybe.Just#`<init>`().*//*=>>Enums.Maybe.Just#copy().[A]*//*=>>Enums.Maybe.Just#copy().(value)*//*=>>Enums.Maybe.Just.apply().[A]*//*=>>Enums.Maybe.Just.apply().(value)*//*=>>Enums.Maybe.Just.unapply().[A]*/case Just/*<<=Enums.Maybe.Just#*/(value/*<<=Enums.Maybe.Just#`<init>`().*//*<<=Enums.Maybe.Just#`<init>`().(value)*//*<<=Enums.Maybe.Just#(value)*//*<<=Enums.Maybe.Just#copy().(value)*//*<<=Enums.Maybe.Just.apply().(value)*/: A/*=>>Enums.Maybe.Just#`<init>`().[A]*//*=>>Enums.Maybe.Just#(value)*/)/*=>>scala.Product#*//*=>>scala.*//*=>>_root_*//*=>>scala.package.Serializable#*/
    /*=>>Enums.Maybe#`<init>`().*/case None/*<<=Enums.Maybe.None.*/
  }

  enum /*=>>java.lang.Object#`<init>`().*/Tag/*<<=Enums.Tag#*/[A] {
/*<<=Enums.Tag#`<init>`().*//*<<=Enums.Tag#`<init>`().[A]*//*<<=Enums.Tag#[A]*//*=>>Enums.Tag.IntTag.*//*=>>Enums.Tag.BooleanTag.*/    /*=>>java.lang.Object#`<init>`().*//*=>>scala.package.Serializable#*//*=>>scala.*//*=>>_root_*/case IntTag/*<<=Enums.Tag.IntTag.*/ extends Tag/*=>>Enums.Tag#*/[Int/*=>>scala.Int#*/]/*=>>Enums.Tag#`<init>`().*/
    case BooleanTag/*<<=Enums.Tag.BooleanTag.*/ extends Tag/*=>>Enums.Tag#*/[Boolean/*=>>scala.Boolean#*/]/*=>>Enums.Tag#`<init>`().*/
  }

  enum /*=>>java.lang.Object#`<init>`().*/=:=/*<<=Enums.`=:=`#*/[A, B]/*<<=Enums.`=:=`#`<init>`().*//*<<=Enums.`=:=`#`<init>`().[A]*//*<<=Enums.`=:=`#[A]*//*<<=Enums.`=:=`#`<init>`().[B]*//*<<=Enums.`=:=`#[B]*/ {
    /*=>>java.lang.Object#`<init>`().*//*=>>scala.package.Serializable#*//*=>>scala.*//*=>>_root_*//*=>>Enums.`=:=`.Refl#`<init>`().*//*=>>Enums.`=:=`.Refl#copy().[C]*//*=>>Enums.`=:=`.Refl.apply().[C]*//*=>>Enums.`=:=`.Refl.unapply().[C]*/case Refl/*<<=Enums.`=:=`.Refl#*/[C]() /*<<=Enums.`=:=`.Refl#`<init>`().*//*<<=Enums.`=:=`.Refl#`<init>`().[C]*//*<<=Enums.`=:=`.Refl#[C]*//*<<=Enums.`=:=`.Refl#copy().[C]*//*<<=Enums.`=:=`.Refl.apply().[C]*//*<<=Enums.`=:=`.Refl.unapply().[C]*/extends (C/*=>>Enums.`=:=`.Refl#[C]*//*=>>Enums.`=:=`.Refl.apply().[C]*/ =:=/*=>>Enums.`=:=`#*/ C/*=>>Enums.`=:=`.Refl#[C]*//*=>>Enums.`=:=`.Refl.apply().[C]*//*=>>Enums.`=:=`#`<init>`().*/)/*=>>scala.Product#*//*=>>scala.*//*=>>_root_*//*=>>scala.package.Serializable#*/
  }

  def unwrap/*<<=Enums.unwrap().*/[A/*<<=Enums.unwrap().[A]*/,B/*<<=Enums.unwrap().[B]*/](opt/*<<=Enums.unwrap().(opt)*/: Option/*=>>scala.Option#*/[A/*=>>Enums.unwrap().[A]*/])(given ev/*<<=Enums.unwrap().(ev)*/: A/*=>>Enums.unwrap().[A]*/ =:=/*=>>Enums.`=:=`#*/ Option/*=>>scala.Option#*/[B/*=>>Enums.unwrap().[B]*/]): Option/*=>>scala.Option#*/[B/*=>>Enums.unwrap().[B]*/] = {
    ev/*=>>Enums.unwrap().(ev)*/ match {
      case =:=/*=>>Enums.`=:=`.*/.Refl/*=>>Enums.`=:=`.Refl.*/() => opt/*=>>Enums.unwrap().(opt)*/.flatMap/*=>>scala.Option#flatMap().*/(identity/*=>>scala.Predef.identity().*/[Option/*=>>scala.Option#*/[B/*=>>Enums.unwrap().[B]*/]])
    }
  }

  enum Planet/*<<=Enums.Planet#*/(mass:/*<<=Enums.Planet#`<init>`().*//*<<=Enums.Planet#`<init>`().(mass)*//*<<=Enums.Planet#(mass)*/ Double/*=>>scala.Double#*/, radius/*<<=Enums.Planet#`<init>`().(radius)*//*<<=Enums.Planet#(radius)*/: Double/*=>>scala.Double#*/) extends java/*=>>java.*/.lang/*=>>java.lang.*/.Enum/*=>>java.lang.Enum#*/[Planet/*=>>Enums.Planet#*/]/*=>>java.lang.Enum#`<init>`().*//*=>>Enums.Planet.Mercury.*//*=>>Enums.Planet.Venus.*//*=>>Enums.Planet.Earth.*//*=>>Enums.Planet.Mars.*//*=>>Enums.Planet.Jupiter.*//*=>>Enums.Planet.Saturn.*//*=>>Enums.Planet.Uranus.*//*=>>Enums.Planet.Neptune.*/ {
    private final val G/*<<=Enums.Planet#G.*/ = 6.67300E-11
    def surfaceGravity/*<<=Enums.Planet#surfaceGravity().*/ = G/*=>>Enums.Planet#G.*/ */*=>>scala.Double#`*`(+6).*/ mass/*=>>Enums.Planet#(mass)*/ //*=>>scala.Double#`.`(+6).*/ (radius/*=>>Enums.Planet#(radius)*/ */*=>>scala.Double#`*`(+6).*/ radius/*=>>Enums.Planet#(radius)*/)
    def surfaceWeight/*<<=Enums.Planet#surfaceWeight().*/(otherMass/*<<=Enums.Planet#surfaceWeight().(otherMass)*/: Double/*=>>scala.Double#*/) = otherMass/*=>>Enums.Planet#surfaceWeight().(otherMass)*/ */*=>>scala.Double#`*`(+6).*/ surfaceGravity/*=>>Enums.Planet#surfaceGravity().*/

    /*=>>java.lang.Object#`<init>`().*//*=>>scala.package.Serializable#*//*=>>scala.*//*=>>_root_*/case Mercury/*<<=Enums.Planet.$values.*//*<<=Enums.Planet.Mercury.*/ extends /*=>>scala.collection.IterableOnceOps#toArray().*//*=>>scala.runtime.EnumValues#values().*//*=>>Enums.Planet.$values.*//*=>>scala.reflect.ClassTag.apply().*//*=>>scala.reflect.ClassTag.*//*=>>scala.runtime.EnumValues#`<init>`().*//*=>>scala.collection.MapOps#apply().*//*=>>scala.runtime.EnumValues#fromName().*//*=>>Enums.Planet.valueOf().($name)*//*=>>`<special-ops>`.throw().*//*=>>java.lang.IllegalArgumentException#`<init>`(+1).*//*=>>java.lang.String#concat().*/Planet/*=>>Enums.Planet#*//*=>>Enums.Planet#`<init>`().*/(3.303e+23, 2.4397e6)/*=>>scala.runtime.EnumValues#register().*//*=>>Enums.Planet.$values.*/
    case Venus/*<<=Enums.Planet.Venus.*/   extends Planet/*=>>Enums.Planet#*//*=>>Enums.Planet#`<init>`().*/(4.869e+24, 6.0518e6)/*=>>scala.runtime.EnumValues#register().*//*=>>Enums.Planet.$values.*/
    case Earth/*<<=Enums.Planet.Earth.*/   extends Planet/*=>>Enums.Planet#*//*=>>Enums.Planet#`<init>`().*/(5.976e+24, 6.37814e6)/*=>>scala.runtime.EnumValues#register().*//*=>>Enums.Planet.$values.*/
    case Mars/*<<=Enums.Planet.Mars.*/    extends Planet/*=>>Enums.Planet#*//*=>>Enums.Planet#`<init>`().*/(6.421e+23, 3.3972e6)/*=>>scala.runtime.EnumValues#register().*//*=>>Enums.Planet.$values.*/
    case Jupiter/*<<=Enums.Planet.Jupiter.*/ extends Planet/*=>>Enums.Planet#*//*=>>Enums.Planet#`<init>`().*/(1.9e+27,   7.1492e7)/*=>>scala.runtime.EnumValues#register().*//*=>>Enums.Planet.$values.*/
    case Saturn/*<<=Enums.Planet.Saturn.*/  extends Planet/*=>>Enums.Planet#*//*=>>Enums.Planet#`<init>`().*/(5.688e+26, 6.0268e7)/*=>>scala.runtime.EnumValues#register().*//*=>>Enums.Planet.$values.*/
    case Uranus/*<<=Enums.Planet.Uranus.*/  extends Planet/*=>>Enums.Planet#*//*=>>Enums.Planet#`<init>`().*/(8.686e+25, 2.5559e7)/*=>>scala.runtime.EnumValues#register().*//*=>>Enums.Planet.$values.*/
    case Neptune/*<<=Enums.Planet.Neptune.*/ extends Planet/*=>>Enums.Planet#*//*=>>Enums.Planet#`<init>`().*/(1.024e+26, 2.4746e7)/*=>>scala.runtime.EnumValues#register().*//*=>>Enums.Planet.$values.*/
  }
}
