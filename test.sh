#!/usr/bin/env bash

# h9cc automatic test script

test() {
    input="$1"
    expect="$2"
    tmpfile_noextension=$(mktemp /tmp/h9cc-test-XXXXX)

    runghc ./Main.hs "$input" > "$tmpfile_noextension.s"
    gcc -o "$tmpfile_noextension" "$tmpfile_noextension.s"
    "$tmpfile_noextension"
    actual="$?"

    if [ "$actual" == "$expect" ]; then
        echo '[[32mPASS[m]' "$input => $actual"
	else
		echo '[[31mFAIL[m]' "$input => expect $expect, but get $actual (see $tmpfile_noextension.s)"
		exit 1
	fi

    rm $tmpfile_noextension{.s,}
}

# test input expect
test 0 0
test 42 42

echo ''
echo 'h9cc test finish.'