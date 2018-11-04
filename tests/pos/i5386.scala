object Test {
  ~{
    println("!")
    1
  }

  +{
    println("!")
    1
  }

  -{
    println("!")
    1
  }

  !{
    println("!")
    true
  }

  !(try true finally{()})
}