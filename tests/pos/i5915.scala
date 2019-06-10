trait RunDSL

val rdsl = new RunDSL {}

delegate RunNNFExpr[B] for RunDSL = rdsl

delegate RunNNFImpl[B] for RunDSL {
  //override def runDSL(b: NNF[B]): B = b.terminal
}