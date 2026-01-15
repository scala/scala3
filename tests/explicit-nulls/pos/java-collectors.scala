import java.util.stream._

def test =
  val seqStream: Stream[String] = ???
  val shouldNotNPE = seqStream.collect(Collectors.toList())
  val shouldNotNPE2 = seqStream.collect(Collectors.toList[String]())