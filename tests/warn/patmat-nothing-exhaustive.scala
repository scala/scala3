enum TestAdt:
  case Inhabited
  case Uninhabited(no: Nothing)

def test1(t: TestAdt): Int = t match
  case TestAdt.Inhabited => 1

def test2(o: Option[Option[Nothing]]): Int = o match
  case Some(None) => 1
  case None => 2
