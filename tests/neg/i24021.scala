@main def Test = {
  (apply = () => 1, k2 = 2).k2 // error
  (apply = () => 1)() // error
}
