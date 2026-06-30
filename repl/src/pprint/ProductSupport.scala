package pprint

object ProductSupport {

  private def isIdentifier(name: String) = {
    def isStart(c: Char) = (c == '_') || (c == '$') || Character.isUnicodeIdentifierStart(c)
    def isPart(c: Char) = (c == '$') || Character.isUnicodeIdentifierPart(c)
    name.toList match {
      case first :: rest if(isStart(first)) =>
        rest.forall(isPart)
      case _ =>
        false
    }
  }

  def treeifyProductElements(x: Product,
                             walker: Walker,
                             escapeUnicode: Boolean,
                             showFieldNames: Boolean): Iterator[Tree] = {
    if (!showFieldNames || x.productArity < 2) {
      x.productIterator.map(x => walker.treeify(x, escapeUnicode, showFieldNames))
    }
    else x.productElementNames
      .zipWithIndex
      .map {
        case (name, i) =>
          val key = 
            if(!isIdentifier(name)) {
              s"`$name`"
            } else {
              name
            }
          val elem = x.productElement(i)
          Tree.KeyValue(key, walker.treeify(elem, escapeUnicode, showFieldNames))
      }
  }

}
