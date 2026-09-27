logLevel := Level.Debug
ThisBuild / incOptions ~= { _.withApiDebug(true) }
ThisBuild / incOptions ~= { _.withRelationsDebug(true) }
