class Box[X](val elem: X)

val runOps = [C^] => (b: Box[() ->{C^} Unit]) => b.elem()
val runOpsCheck: [C^] -> (ops: List[() ->{C^} Unit]) ->{C^} Unit = runOps  // error
val runOpsCheck2: [C^] => (ops: List[() ->{C^} Unit]) ->{C^} Unit = runOps // error


