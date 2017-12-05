package scala.quoted

import scala.runtime.quoted.Unpickler.Pickled

/** A quote backed by a pickled TASTY tree */
trait TastyQuoted extends Quoted {
  def tasty: Pickled
  def args: Seq[Any]
}
