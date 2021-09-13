class C {
  val bool: true      = true
  val not1: None.type = None

  def t1 = true match { case true => "inline true" }
  def t2 = bool match { case true => "valdef true" }
  def t3 = None match { case None => "inline None" }
  def t4 = not1 match { case None => "valdef None" }

  val monday: Day.Mon.type = Day.Mon
  val someday: Day         = Day.Mon

  def t5 = Day.Mon match { case Day.Mon => 1 case Day.Tue => 2 case Day.Wed => 3 }
  def t6 = monday  match { case Day.Mon => 1 case Day.Tue => 2 case Day.Wed => 3 }
  def t7 = someday match { case Day.Mon => 1 case Day.Tue => 2 case Day.Wed => 3 }
}

enum Day { case Mon, Tue, Wed, Thu, Fri }
