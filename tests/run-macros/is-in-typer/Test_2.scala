
@main def Test =
  assert(!isWhileTyping)
  assert(isWhileTypingTransparent)
  assert(f1 == "afterTyper")
  assert(f2 == "afterTyper")
  assert(f3 == "inTyper")
  assert(f4 == "inTyper")

inline def f1 =
   inline if isWhileTyping then "inTyper" else "afterTyper"

inline def f2 =
  inline if isWhileTypingTransparent /*delayed*/ then "inTyper" else "afterTyper"

transparent inline def f3 =
  inline if isWhileTyping /*forced*/ then "inTyper" else "afterTyper"

transparent inline def f4 =
  inline if isWhileTypingTransparent then "inTyper" else "afterTyper"
