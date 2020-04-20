#!/usr/bin/env bash

# h9cc automatic test script

test() {
    input="$1"
    expect="$2"
    tmpfile_noextension=$(mktemp /tmp/h9cc-test-XXXXX)

    # ghc will only search for current working directory for modules,
    # so we have to use `-i⟨dir⟩[:⟨dir⟩]*` to add import directory (see `man ghc`)
    runghc -i./src ./src/Main.hs "$input" > "$tmpfile_noextension.s"
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
test "0;" 0
test "42;" 42
test "9+3;" 12
test "5-2;" 3
test "10+3+2;" 15
test "5+20-4;" 21
test " 12 + 34 - 5 ;" 41
test "5+6*7;" 47
test "5*(9-6);" 15
test "(3+5)/2;" 4
test "1*2+(3+4)/5+6*7/8-(9-10);" 9
test "1 *    2+(3+ 4)/5+ 6*7/8 -(9-10);" 9
test "-3*+5+100;" 85
test "-10+20;" 10
test "1+ -2 + +3*(-4)+5 + 200;" 192
test "2 + 3 >= 5 == 5-4 < 0 != 1;" 1
test "1+2/3;4>5;6==9;" 0
test "a=3;b=a+9;b;" 12
test "a = 3; b = 5 * 6 - 8; return a + b / 2; 99;" 14
test "return 5; return 8;" 5

echo ''
echo 'h9cc test finish.'