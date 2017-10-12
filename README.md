# Scala TicTacToe
[![Build Status](https://travis-ci.org/kyle-annen/scala-tictactoe.svg?branch=master)](https://travis-ci.org/kyle-annen/scala-tictactoe)
[![Coverage Status](https://coveralls.io/repos/github/kyle-annen/scala-tictactoe/badge.svg?branch=scoverage)](https://coveralls.io/github/kyle-annen/scala-tictactoe?branch=scoverage)
[![Codacy Badge](https://api.codacy.com/project/badge/Grade/068dd8bb9d70458685228ec9fab3c164)](https://www.codacy.com/app/kyle-annen/scala-tictactoe?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=kyle-annen/scala-tictactoe&amp;utm_campaign=Badge_Grade)

# Requirements

- Java 8
- Scala 2.12
- install sbt

To run, clone repository and run the following command from the root directory of the cloned repository.
``` bash
sbt run
```


# Test Coverage

To run test coverage 

``` bash
sbt clean coverage test
```

To generate coverage report 

``` bash
sbt coverageReport
```

The test coverage reports are located here:

``` 
target/scala-2.XX/scoverage-report
```

# Clojars deploy

From the project root, run in this order:


Have sbt compile the target directory.

``` bash
sbt package 
```

Have sbt generate the pom.

``` bash
sbt makePom
```

Change directory

``` bash
cd target
```

Copy the compiled POM to pom.xml in target directory

``` bash
cp tictactoe-X.X.X-SNAPSHOT.pom ../pom.xml
```

Move back to the root directory.

``` bash
cd ..
```

Deploy with Maven.

``` bash
mvn deploy
```
