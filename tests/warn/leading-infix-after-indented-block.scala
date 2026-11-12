// A leading infix operator that is less indented than a preceding indented block
// is nevertheless applied to that block. Warn, since the layout suggests otherwise.

def c = true

def ifElse =
  if c then
    1
  else
    2
  + 100 // warn

def ifThen =
  if c then
    1
  * 100 // warn
  else 2

def whileDo =
  while c do
    ()
  == () // warn

def forYield =
  for x <- List(1) yield
    x
  + 1 // warn

def tryFinally =
  try 1
  finally
    "a"
  + "b" // warn

def tryCatch =
  try 1
  catch case _: Exception =>
    2
  + 100 // warn

def lambda =
  val f = (x: Int) =>
    x
  + 100 // warn
  f

// a hanging operator at top level stays inside the lambda body,
// no outdent is inserted, so this is not affected
def lambdaHanging = (x: Int) =>
    x
  + 100

def assign =
  var v = 0
  v =
    1
  + 100 // warn
  v

def nested =
  if c then
    1
  else
    if c then
      2
    else
      3
    + 100 // warn

// no warnings below

def indentedOp =
  if c then
    1
  else
    2
    + 100

def sameLine =
  if c then
    1
  else
    2 + 100

def braces =
  if c then
    1
  else {
    2
  }
  + 100

def colonArg =
  identity:
    1
  + 100

def hangingOp =
  val x =
      1
    + 100
  x

def separateStatement =
  if c then
    println()

  + 100
