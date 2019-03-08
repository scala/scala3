trait RunDSL

val rdsl = new RunDSL {}

implied RunNNFExpr[B] for RunDSL = rdsl

implied RunNNFImpl[B] for RunDSL {
  //override def runDSL(b: NNF[B]): B = b.terminal
}