object Test {
  type Const[t] = { type L[x] = t }
  type Bar[a] = Const[Int]#L[a]
}
