class Box[X](val elem: X)

val runOps = [cap C] => (b: Box[() ->{C} Unit]) => b.elem()
val runOpsCheck: [cap C] -> (ops: List[() ->{C} Unit]) ->{C} Unit = runOps  // error
val runOpsCheck2: [cap C] => (ops: List[() ->{C} Unit]) ->{C} Unit = runOps // error


