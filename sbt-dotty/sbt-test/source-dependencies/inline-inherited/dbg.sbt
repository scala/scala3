logLevel := Level.Debug
incOptions in ThisBuild ~= { _.withApiDebug(true) }
incOptions in ThisBuild ~= { _.withRelationsDebug(true) }
