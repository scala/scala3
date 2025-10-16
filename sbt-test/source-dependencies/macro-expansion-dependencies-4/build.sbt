name := "add-dep"
libraryDependencies ++= Seq(
  "com.softwaremill.macwire" %% "macros" % "2.6.6" % Provided,
  "com.softwaremill.macwire" %% "util"   % "2.6.6",
)
Compile / incOptions ~= { _.withRecompileAllFraction(1.0) }
