trait Document[D]
trait DocumentModel[D <: Document[D]]

class Store[Doc <: Document[Doc], Model <: DocumentModel[Doc]]
class OpenSearchStore[Doc <: Document[Doc], Model <: DocumentModel[Doc]] extends Store[Doc, Model]
class SplitCollection[Doc <: Document[Doc], Model <: DocumentModel[Doc], Searching <: Store[Doc, Model]](val searching: Searching)
  extends Store[Doc, Model]

def candidates(store: Store[_, _]) =
  store match {
    case sc: SplitCollection[_, _, _] =>
      sc.searching match {
        case os: OpenSearchStore[_, _] => os
        case _ => null
      }
  }
