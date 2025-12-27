package scala.quoted.compiletime

import scala.quoted.compiletime as pub

trait Quotes {

  lazy val reflectV2: pub.reflect.Module

}
