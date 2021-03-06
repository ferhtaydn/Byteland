logLevel := Level.Warn

addSbtPlugin("com.typesafe.sbt" % "sbt-scalariform" % "1.3.0")

resolvers += "sonatype-releases" at "https://oss.sonatype.org/content/repositories/releases/"

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.7.0")