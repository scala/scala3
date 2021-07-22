package scala.reflect

import scala.reflect.api

package object runtime {

  lazy val universe: api.JavaUniverse = new api.JavaUniverse {}

}
