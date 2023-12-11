package com.example

import org.jooq.impl.AbstractRoutine

// Works in Scala 2.12 and 2.13 but is broken in Scala 3
class MyRoutineScala extends AbstractRoutine[String] {}
