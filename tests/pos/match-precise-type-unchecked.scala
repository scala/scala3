import scala.collection.*
import scala.util.*

val (ss: Seq[Success[Int] @unchecked], fs: Seq[Failure[Int] @unchecked]) =
  Seq.empty[Try[Int]].partition(_.isSuccess).runtimeChecked

val (ss2: Seq[Success[Int]], fs2: Seq[Failure[Int]]) = Seq.empty[Try[Int]].partition(_.isSuccess).runtimeChecked match
  case (s: Seq[Success[Int] @unchecked], f: Seq[Failure[Int] @unchecked]) => (s, f)

val (ss3: Seq[Success[Int]], fs3: Seq[Failure[Int]]) = Seq.empty[Try[Int]].partition(_.isSuccess).runtimeChecked match
  case x @ (s: Seq[Success[Int] @unchecked], f: Seq[Failure[Int] @unchecked]) => x

val (ss4, fs4: Seq[Failure[Int]]) = Seq.empty[Try[Int]].partition(_.isSuccess).runtimeChecked match
  case x @ (_, f: Seq[Failure[Int] @unchecked]) => x

