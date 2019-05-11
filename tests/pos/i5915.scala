trait RunDSL

val rdsl = new RunDSL {}

implicit RunNNFExpr[B] for RunDSL = rdsl

implicit RunNNFImpl[B] for RunDSL {
  //override def runDSL(b: NNF[B]): B = b.terminal
}