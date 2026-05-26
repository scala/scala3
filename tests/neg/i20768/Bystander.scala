//> using options -source:future

import cats.Eval
import cats.data.StateT

case class Bystander()

object Bystander:
  def of(index: Int): StateT[Eval, Unit, Bystander] =
    StateT.get.flatMap { partialResultsCache =>
      _of(index)
        .flatMap(computedResult =>
          StateT
            .set(???) // error
            >> StateT.pure(computedResult) // error
        )
    }

  def _of(index: Int): StateT[Eval, Unit, Bystander] = ???
