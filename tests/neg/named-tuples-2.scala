def Test =
  val person = (name = "Bob", age = 33, married = true)
  person match // warn
    case (name, age) => () // error
    case (n, a, m, x) => () // error
