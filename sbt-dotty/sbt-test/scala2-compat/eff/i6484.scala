import cats._
import cats.data._
import cats.implicits._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

import scala.language.implicitConversions

object Test {
  def resolve[T](e: Eff[Fx1[[X] => Kleisli[Id, String, X]], T], g: String): T = 
    e.runReader[String](g)(Member.Member1[[X] => Kleisli[Id, String, X]]).run
}
