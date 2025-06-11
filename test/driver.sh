#!/bin/bash
tmp=`mktemp -d /tmp/zcc-test-XXXXXX`
trap 'rm -rf $tmp' INT TERM HUP EXIT
echo > $tmp/empty.c

check() {
    if [ $? -eq 0 ]; then
        echo "testing $1 ... passed"
    else
        echo "testing $1 ... failed"
        exit 1
    fi
}

# -o
rm -f $tmp/out
./zcc -o $tmp/out $tmp/empty.c
[ -f $tmp/out ]
check -o

# --help
./zcc --help 2>&1 | grep -q zcc
check --help

# -S
echo 'int main() {}' | ./zcc -S -o - - | grep -q 'main:'
check -S

# Default output file
rm -f $tmp/out.o $tmp/out.s
echo 'int main() {}' > $tmp/out.c
(cd $tmp; $OLDPWD/zcc out.c)
[ -f $tmp/out.o ]
check 'default output file'

(cd $tmp; $OLDPWD/zcc -S out.c)
[ -f $tmp/out.s ]
check 'default output file'

echo OK
