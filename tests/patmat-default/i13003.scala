case class One(two: Two)
case class Two(o: Option[Int])

def matchOneTwo(one: One) = one match
    case One(Two(Some(i))) => "match!"

def matchTwo(two: Two) = two match
    case Two(Some(i)) => "match!"

def matchOO(oo: Option[Option[Int]]) = oo match
    case Some(Some(i)) => "match!"

def matchOOO(ooo: Option[Option[Option[Int]]]) = ooo match
    case Some(Some(Some(i))) => "match!"
