#!/usr/bin/env bin/scala

// This file is a Scala CLI script.

println(new java.sql.Date(100L))
System.err.println("SCALA_OPTS="+Option(System.getenv("SCALA_OPTS")).getOrElse(""))
