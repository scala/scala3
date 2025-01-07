trait RunDSL

val rdsl = new RunDSL {}

given RunNNFExpr: [B] => RunDSL = rdsl

given RunNNFImpl: [B] => RunDSL {
  //override def runDSL(b: NNF[B]): B = b.terminal
}