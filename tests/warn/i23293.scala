trait SelectByName[Field <: String & Singleton, Rec <: Tuple]:
  type Out
  extension (r: Rec) def apply[F <: Field]: Out // warn not crash
