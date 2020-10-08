    /*val x33: String => String = x22 => x22 match {
      case "abc" => ""
      case x34 => x34
    }*/
    val y: PartialFunction[String, String] = x => x match {
      case "abc" => ""
      case _ => x
    }

