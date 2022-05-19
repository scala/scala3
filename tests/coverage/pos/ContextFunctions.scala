package covtest

class OnError(op: Exception => Any):
  def onError(fallback: Any): Any = fallback

class Imperative:

  def readName2 = (t: Exception) ?=> (c: String) ?=> {
    "name"
  }

  def readPerson(s: String) =
    val e: Exception = null
    OnError((e) => readName2(using e)(using s)).onError(None)
