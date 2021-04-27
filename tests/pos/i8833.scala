@main def Test = {
  implicitly[("k" | "v") <:< ("k" | "v")]
  implicitly[("k" | "v") =:= ("k" | "v")]
}
