import collection.mutable
class Suppression:
  def matches(f: SourceFile): Boolean = ???
class SourceFile
private val mySuppressions: mutable.LinkedHashMap[SourceFile, mutable.ListBuffer[Suppression]] = mutable.LinkedHashMap.empty

def test(f: SourceFile) =
  mySuppressions.getOrElse(f, Nil).find(_.matches(f))