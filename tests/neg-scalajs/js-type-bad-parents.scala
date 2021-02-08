import scala.language.dynamics

import scala.scalajs.js
import scala.scalajs.js.annotation.*

// AnyRef as direct parent, invalid for non-native JS types, except traits

trait A1 extends js.Any // ok for trait
class A2 extends js.Any // error
object A3 extends js.Any // error

trait A4 extends AnyRef with js.Any // ok for trait
class A5 extends AnyRef with js.Any // error
object A6 extends AnyRef with js.Any // error

trait A7 extends Object with js.Any // ok for trait
class A8 extends Object with js.Any // error
object A9 extends Object with js.Any // error

// Native JS trait as parent, invalid for non-native JS types

@js.native
trait NativeJSTrait extends js.Any

trait B1 extends js.Object with NativeJSTrait // error
class B2 extends js.Object with NativeJSTrait // error
object B3 extends js.Object with NativeJSTrait // error

// Scala class or trait, invalid

class ScalaClass
trait ScalaTrait

trait C1 extends ScalaClass with js.Any // error
class C2 extends ScalaClass with js.Any // error
object C3 extends ScalaClass with js.Any // error

@js.native
trait C4 extends ScalaClass with js.Any // error
@js.native @JSGlobal
class C5 extends ScalaClass with js.Any // error
@js.native @JSGlobal
object C6 extends ScalaClass with js.Any // error

trait C7 extends js.Object with ScalaTrait // error
class C8 extends js.Object with ScalaTrait // error
object C9 extends js.Object with ScalaTrait // error

@js.native
trait C10 extends js.Object with ScalaTrait // error
@js.native @JSGlobal
class C11 extends js.Object with ScalaTrait // error
@js.native @JSGlobal
object C12 extends js.Object with ScalaTrait // error

// Exception: OK to extend scala.Dynamic


trait D1 extends js.Object with scala.Dynamic
class D2 extends js.Object with scala.Dynamic
object D3 extends js.Object with scala.Dynamic

@js.native
trait D4 extends js.Object with scala.Dynamic
@js.native @JSGlobal
class D5 extends js.Object with scala.Dynamic
@js.native @JSGlobal
object D6 extends js.Object with scala.Dynamic
