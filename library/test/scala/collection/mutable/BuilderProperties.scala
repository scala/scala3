package scala.collection.mutable

class BuilderProperties

import org.scalacheck.Arbitrary.arbInt
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.commands.Commands

import scala.collection.mutable
import scala.util.{Success, Try}

/** Generic stateful property testing for builders
  */
class SeqBuilderStateProperties[A: Arbitrary, To <: Seq[A]](newBuilder: => mutable.Builder[A, To])(arbA: Arbitrary[A]) extends Commands {

  override type State = List[A]
  override type Sut = mutable.Builder[A, To]

  override def genInitialState: Gen[State] = Nil

  override def canCreateNewSut(
    newState: State,
    initSuts: scala.Iterable[State],
    runningSuts: scala.Iterable[Sut]) = true

  override def newSut(state: State) = newBuilder.addAll(state)

  override def destroySut(sut: Sut): Unit = ()

  override def initialPreCondition(state: State) = state.isEmpty

  override def genCommand(state: State): Gen[Command] = {
    import Gen._
    Gen.oneOf(
      const(Clear),
      const(Result),
      arbInt.arbitrary.map(SizeHint.apply),
      arbA.arbitrary.map(a => AddOne(a)),
      listOf(arbA.arbitrary).map(a => AddAll(a))
    )
  }

  case object Clear extends UnitCommand {
    override def postCondition(state: State, success: Boolean) = success
    override def run(sut: Sut) = sut.clear()
    override def nextState(state: State) = Nil
    override def preCondition(state: State) = true
  }
  case object Result extends Command {
    override type Result = To
    override def postCondition(state: State, result: Try[Result]) = result == Success(state.reverse)
    override def run(sut: Sut) = sut.result()
    override def nextState(state: State) = state
    override def preCondition(state: State) = true
  }
  case class SizeHint(size: Int) extends UnitCommand {
    override def postCondition(state: State, success: Boolean) = success
    override def run(sut: Sut) = sut.sizeHint(size)
    override def nextState(state: State) = state
    override def preCondition(state: State) = true
  }
  case class AddOne(elem: A) extends UnitCommand {
    override def postCondition(state: State, success: Boolean) = success
    override def run(sut: Sut) = sut.addOne(elem)
    override def nextState(state: State) = elem :: state
    override def preCondition(state: State) = true
  }
  case class AddAll(elems: scala.collection.immutable.Seq[A]) extends UnitCommand {
    override def postCondition(state: State, success: Boolean) = success
    override def run(sut: Sut) = sut.addAll(elems)
    override def nextState(state: State) = state.prependedAll(elems)
    override def preCondition(state: State) = true
  }
}
