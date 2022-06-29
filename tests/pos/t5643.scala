object TupledEvidenceTest {

  abstract class TupledEvidence[M[_], T0] { type T = T0 }

  implicit def witnessTuple2[M[_], T1, T2](implicit ev1: M[T1], ev2: M[T2]):
    TupledEvidence[M, (T1, T2)] { type T = (T1, T2) } = sys.error("")

  class GetResult[T]

  implicit val getString: GetResult[String] = new GetResult[String]

  implicit def getTuple[T](implicit w: TupledEvidence[GetResult, T]): GetResult[w.T] = sys.error("")

  def f[T : GetResult] = ""

  f[(String,String)](using getTuple[(String, String)])

  f[(String,String)]
}
