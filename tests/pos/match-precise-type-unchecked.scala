import scala.collection.*
import scala.util.*

val (ss: Seq[Success[Int] @unchecked], fs: Seq[Failure[Int] @unchecked]) =
  Seq.empty[Try[Int]].partition(_.isSuccess)

val (ss2: Seq[Success[Int]], fs2: Seq[Failure[Int]]) = Seq.empty[Try[Int]].partition(_.isSuccess) match
  case (s: Seq[Success[Int] @unchecked], f: Seq[Failure[Int] @unchecked]) => (s, f)

val (ss3: Seq[Success[Int]], fs3: Seq[Failure[Int]]) = Seq.empty[Try[Int]].partition(_.isSuccess) match
  case x @ (s: Seq[Success[Int] @unchecked], f: Seq[Failure[Int] @unchecked]) => x
