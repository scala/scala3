package example

import scala.language/*=>>scalaShadowing.language.*/.implicitConversions/*=>>scalaShadowing.language.implicitConversions.*/

class ImplicitConversion/*<<=example.ImplicitConversion#*/ {
  implicit def string2Number/*<<=example.ImplicitConversion#string2Number().*/(
      string/*<<=example.ImplicitConversion#string2Number().(string)*/: String/*=>>scala.Predef.String#*/
  ): Int/*=>>scala.Int#*/ = 42
  implicit def newAny2StringAdd/*<<=example.ImplicitConversion#newAny2StringAdd().*/[T/*<<=example.ImplicitConversion#newAny2StringAdd().[T]*/](
      any/*<<=example.ImplicitConversion#newAny2StringAdd().(any)*/: T/*=>>example.ImplicitConversion#newAny2StringAdd().[T]*/
  ): Predef/*=>>scala.Predef.*/.any2stringadd/*=>>scala.Predef.any2stringadd#*/[T/*=>>example.ImplicitConversion#newAny2StringAdd().[T]*/] = new Predef/*=>>scala.Predef.*/.any2stringadd/*=>>*//*=>>scala.Predef.any2stringadd#`<init>`().*/(any/*=>>example.ImplicitConversion#newAny2StringAdd().(any)*/)
  val message/*<<=example.ImplicitConversion#message.*/ = ""
  val number/*<<=example.ImplicitConversion#number.*/ = 42
  val tuple/*<<=example.ImplicitConversion#tuple.*/ = (1, 2)
  val char/*<<=example.ImplicitConversion#char.*/: Char/*=>>scala.Char#*/ = 'a'

  // extension methods
  /*=>>scala.Predef.augmentString().*/message/*=>>example.ImplicitConversion#message.*/
    .stripSuffix/*=>>scala.collection.StringOps#stripSuffix().*/("h")
  /*=>>example.ImplicitConversion#newAny2StringAdd().*/tuple/*=>>example.ImplicitConversion#tuple.*/ +/*=>>scala.Predef.any2stringadd#`+`().*/ "Hello"

  // implicit conversions
  val x/*<<=example.ImplicitConversion#x.*/: Int/*=>>scala.Int#*/ = /*=>>example.ImplicitConversion#string2Number().*/message/*=>>example.ImplicitConversion#message.*/

  // interpolators
  s"Hello $message/*=>>example.ImplicitConversion#message.*/ $number/*=>>example.ImplicitConversion#number.*//*=>>scala.StringContext#s().*/"
  /*=>>scala.Predef.augmentString().*/s"""Hello
     |$message/*=>>example.ImplicitConversion#message.*/
     |$number/*=>>example.ImplicitConversion#number.*//*=>>scala.StringContext#s().*/""".stripMargin/*=>>scala.collection.StringOps#stripMargin(+1).*/

  val a/*<<=example.ImplicitConversion#a.*/: Int/*=>>scala.Int#*/ = /*=>>scala.Char.char2int().*/char/*=>>example.ImplicitConversion#char.*/
  val b/*<<=example.ImplicitConversion#b.*/: Long/*=>>scala.Long#*/ = /*=>>scala.Char.char2long().*/char/*=>>example.ImplicitConversion#char.*/
}
