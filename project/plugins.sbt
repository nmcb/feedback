// nmcb SbtRss example resolvers += Resolver.url(
// nmcb SbtRss example   url("http://dl.bintray.com/marco/sbt-plugins"))(
// nmcb SbtRss example   "marco sbt-plugins",
// nmcb SbtRss example   Resolver.ivyStylePatterns)

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.6.0")

addSbtPlugin("org.playground" % "sbt-rss" % "6.6.9")
