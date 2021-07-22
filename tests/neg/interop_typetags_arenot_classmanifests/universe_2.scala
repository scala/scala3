package scala.reflect

package object runtime {

  lazy val universe: api.Universe = new api.Universe {}

}
