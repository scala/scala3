import scala.quoted.*

package mylib:
  object Export:
    transparent inline def exported: Any = 1

  object Import:
    transparent inline def imported: Any = 1

  export Export.*


import mylib.*
import Import.*
object Test:
  val oneFail : 1 = exported //error
  val oneWork : 1 = imported //works
