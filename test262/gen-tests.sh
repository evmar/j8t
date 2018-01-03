#!/bin/sh

j8t=../target/release/j8t

for t in test262/test/language/**/*.js; do
    meta=$(sed -ne '/^\/\*---/,/^---\*\//p' $t)
    if echo "$meta" | grep -q 'phase:'; then
        # 'phase' annotation indicates the file should not load.
        continue
    fi
    echo $t
    out=out/$t
    mkdir -p $(dirname $out)
    echo "$meta" > $out
    $j8t $t 2>&1 >> $out
    if [ $? != 0 ]; then
        rm $out
    fi
done
