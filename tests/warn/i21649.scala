//> using options -explain
// do warn if function literal adaptation has match in non-empty block

import scala.util.*
import PartialFunction.condOpt

val f: PartialFunction[String, Boolean] = _.toUpperCase match { case "TRUE" => true }

val g: PartialFunction[String, Boolean] = x =>
  val uc = x.toUpperCase
  uc match // warn
  case "TRUE" => true

def kollekt = List("1", "two", "3").collect(x => Try(x.toInt) match { case Success(i) => i })

def lesen = List("1", "two", "3").flatMap(x => condOpt(Try(x.toInt)) { case Success(i) => i })
