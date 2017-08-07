# Scala TicTacToe
[![Build Status](https://travis-ci.org/kyle-annen/scala-tictactoe.svg?branch=master)](https://travis-ci.org/kyle-annen/scala-tictactoe)

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
