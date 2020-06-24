@main def Test =
  println(ofMapKeyValues.toList.sortBy(_._1))
  println(ofMapKeys.toList.sortBy(_._1))
  println(ofMapValues.toList.sortBy(_._1))
