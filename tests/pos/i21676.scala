def Test =
  val members = collection.immutable.SortedSet.empty[String]
  members.collect {
    var upNumber = 0
    {
      case m =>
        // upNumber += 1
        m
    }
  }

  members.collect {
    var upNumber = 0
    {
      m => m
    }
  }

