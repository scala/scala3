enum Day extends java.lang.Enum[Day] {
  case MONDAY, TUESDAY, SATURDAY, WEDNESDAY
}

object Test {
  def main(args: Array[String]): Unit = {
    val allDays = java.util.EnumSet.allOf(classOf[Day])
    val dayMap = new java.util.EnumMap[Day, String](classOf[Day])
    dayMap.put(Day.MONDAY, "workday")
    dayMap.put(Day.SATURDAY, "weekend")

    allDays.toArray.foreach(println)
    println(dayMap.get(Day.MONDAY))
    println(dayMap.get(Day.SATURDAY))
  }
}
