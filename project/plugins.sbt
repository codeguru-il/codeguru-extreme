logLevel := Level.Warn

resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.0")

addSbtPlugin("com.codacy" % "sbt-codacy-coverage" % "1.3.7")

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.8.0")

addSbtPlugin("de.johoop" % "findbugs4sbt" % "1.4.0")