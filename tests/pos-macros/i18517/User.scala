package dummy

trait User:
  final def bar(cell:Any) : Unit =
    (cell: cell.type) match
      case c: (Caller & cell.type) => ()
