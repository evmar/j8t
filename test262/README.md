# test262 test suite

These scripts run the test262 test suite inputs through j8t and then run
the test suite against the result.  The idea is that if the test suite passes
before, it should also pass after, and also that the test suite includes
many obscure JS patterns.

## Setup

```sh
$ yarn
```

to download the test harness.

## Run

```
./gen-tests.sh
```

to run all the tests through j8t, then

```
./run-tests.sh
```

to run the test harness.
