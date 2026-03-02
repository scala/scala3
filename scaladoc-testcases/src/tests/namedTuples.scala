package tests.namedTuples

import language.experimental.namedTuples
import NamedTuple.*

type Person = (name: String, age: Int)

type Person2 = NamedTuple[("age2", "name2"), (Int, String)] //expected: type Person2 = (age2: Int, name2: String)

val x = (name = "Bob", age = 25) //expected: val x: (name: String, age: Int)

def foo(p1: (age: Int, name: String), p2: (name: String, age: Int)): Nothing
  = ???

def invalid1: NamedTuple[("age", String), (Int, Int)]
  = ???

def invalid2: NamedTuple[("age", "name"), (Int, Int, Int)]
  = ???

def invalid3: NamedTuple[("age", "name", "something"), (Int, Int)]
  = ???

