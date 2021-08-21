---
title: "Dropped: Do-While"
type: section
num: 75
previous-page: /scala3/reference/dropped-features/type-projection
next-page: /scala3/reference/dropped-features/procedure-syntax
---

The syntax construct
```scala
do <body> while <cond>
```
is no longer supported. Instead, it is recommended to use the equivalent `while` loop
below:
```scala
while ({ <body> ; <cond> }) ()
```
For instance, instead of
```scala
do
  i += 1
while (f(i) == 0)
```
one writes
```scala
while
  i += 1
  f(i) == 0
do ()
```
The idea to use a block as the condition of a while also gives a solution
to the "loop-and-a-half" problem. Here is another example:
```scala
while
  val x: Int = iterator.next
  x >= 0
do print(".")
```

### Why Drop The Construct?

 - `do-while` is used relatively rarely and it can be expressed faithfully using just `while`. So there seems to be little point in having it as a separate syntax construct.
 - Under the [new syntax rules](../other-new-features/control-syntax.html) `do` is used as a statement continuation, which would clash with its meaning as a statement introduction.
