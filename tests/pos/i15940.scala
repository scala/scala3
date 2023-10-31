object Ops:
  implicit class EitherSeqOps[E, T](private val seq: Seq[Either[E, T]]) extends AnyVal:
    def sequence: Either[::[E], Seq[T]] = ???

trait BuildException
case class CompositeBuildException(ex: ::[BuildException]) extends BuildException

trait ActionableDiagnostic
trait ActionableHandler[A <: ActionableDiagnostic]:
  def exec: Either[BuildException, Seq[A]]

import Ops._

val test: Either[BuildException, Seq[ActionableDiagnostic]] =
  // Can be replaced with Seq[Either[BuildException, Seq[ _ <: ActionableDiagnostic]]] , but current version matches better type of missing implicit
  Seq.empty[ActionableHandler[?]].map(_.exec)
    .sequence
    .left.map(_.head)
    .map(_.flatten) // error