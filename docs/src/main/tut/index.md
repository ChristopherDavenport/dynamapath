---
layout: home

---

# dynamapath - Dynamic Path Parsing [![Build Status](https://travis-ci.com/ChristopherDavenport/dynamapath.svg?branch=master)](https://travis-ci.com/ChristopherDavenport/dynamapath) [![Maven Central](https://maven-badges.herokuapp.com/maven-central/io.chrisdavenport/dynamapath_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/io.chrisdavenport/dynamapath_2.12)

## Quick Start

To use dynamapath in an existing SBT project with Scala 2.11 or a later version, add the following dependencies to your
`build.sbt` depending on your needs:

```scala
libraryDependencies ++= Seq(
  "io.chrisdavenport" %% "dynamapath" % "<version>"
)
```

## Parameters

### Named Parameters

Named parameters are defined by prefixing a colon to the parameter name (`:foo`).

### Parameter Modifiers

#### Optional

Parameters can be suffixed with a question mark (`?`) to make the parameter optional (`:foo?`).

#### Zero or more

Parameters can be suffixed with an asterisk (`*`) to denote a zero or more parameter matches (`:foo*`).

#### One or more

Parameters can be suffixed with a plus sign (`+`) to denote a one or more parameter matches (`:foo+`).