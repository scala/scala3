import scala.reflect.Selectable.reflectiveSelectable

def saveRedir(what: {def validate: List[String]}) =
  what.validate match
    case Nil => ???
    case xs  => ???
