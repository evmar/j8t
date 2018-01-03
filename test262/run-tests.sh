#!/bin/sh

exec node_modules/.bin/test262-harness \
    --includesDir test262/harness/ \
    "out/test262/**"
