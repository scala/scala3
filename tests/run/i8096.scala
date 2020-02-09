def k: String =
  try
    "WUT"
  catch case t =>
    println(s"Caught a case of $t")
  "OK"

@main def Test =
  assert(k == "OK")

