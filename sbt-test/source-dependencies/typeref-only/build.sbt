logLevel := Level.Debug

// disable recompile all which causes full recompile which
// makes it more difficult to test dependency tracking
incOptions := Def.uncached(incOptions.value.withRecompileAllFraction(1.0))