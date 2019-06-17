trait Test {
  type Nil = [K] =>> K
  type StrangeCons[H, Tail <: [H, A] =>> H] = Tail[H, H]

  def list: StrangeCons[Int, Nil] // error
}
