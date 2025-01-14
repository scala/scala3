type MonthNumber =
  1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12

def main =
  List[(String, MonthNumber)](
      "January" -> 1,
      "February" -> 2,
      "March" -> 3,
      "April" -> 4,
      "May" -> 5,
      "June"-> 6,
      "July" -> 7,
      "August" -> 8,
      "September" -> 9,
      "October" -> 10,
      "November" -> 11,
      "December" -> 12
    ).foreach { (name, number) =>
      summon[number.type <:< MonthNumber]
    }
