package dotty.tools.dotc.core

import Types._, Contexts._, Symbols._

class Definitions(implicit ctx: Context) {
  lazy val AnyClass: ClassSymbol = ???
  lazy val AnyType = AnyClass.typeConstructor
  lazy val AnyValClass: ClassSymbol = ???
  lazy val NothingClass: ClassSymbol = ???
  lazy val NothingType = NothingClass.typeConstructor
  lazy val NullClass: ClassSymbol = ???
  lazy val NullType = NullClass.typeConstructor
  lazy val ObjectClass: ClassSymbol = ???
  lazy val ObjectType = ObjectClass.typeConstructor
  lazy val SingletonClass: ClassSymbol = ???
  lazy val SingletonType = SingletonClass.typeConstructor
}