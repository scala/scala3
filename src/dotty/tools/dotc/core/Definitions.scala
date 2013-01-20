package dotty.tools.dotc.core

import Types._, Contexts._, Symbols._

class Definitions(implicit ctx: Context) {
  private var _isInitialized = false
  def isInitialized = _isInitialized

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
  lazy val ArrayClass: ClassSymbol = ???
  lazy val uncheckedStableClass: ClassSymbol = ???

  def init() =
    if (!isInitialized) {
      // force initialization of every symbol that is synthesized or hijacked by the compiler
      val forced = ???
      _isInitialized = true
    }
}