sealed trait Fails
case class VarArgs1(a: String*) extends Fails
case class FailsChild2(a: Seq[String]) extends Fails
object FailsTest {
  def matchOnVarArgsFirstFails(f: Fails) = {
    f match {
      case VarArgs1(_*) => ???
      // BUG: Without this line we should get a non-exhaustive match compiler error.
      //case FailsChild2(_) => ???
    }
  }

  def matchOnSeqArgsFirstWorks(f: Fails) = {
    f match {
      case FailsChild2(_) => ???
      // Without this line, the compiler reports a "match may not be exhaustive" error as expected.
      // case VarArgs1(_) => ???
    }
  }
}

sealed trait Works
case class SeqArgs1(a: Seq[String]) extends Works
case class SeqArgs2(a: Seq[String]) extends Works
object WorksTest {
  def matcher(f: Works) = {
    f match {
      case SeqArgs1(_) => ???
      // Without this line, the compiler reports a "match may not be exhaustive" error as expected.
      // case SeqArgs2(_) => ???
    }
  }
}