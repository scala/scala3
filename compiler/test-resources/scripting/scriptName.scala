#!/usr/bin/env scala

  def main(args: Array[String]): Unit =
    val name = sys.props("script.name")
    printf("script.name: %s\n",name)
    assert(name == "scriptName.scala")
