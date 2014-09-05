#!/bin/bash

. ./conf

MBASE="$MONO ./mb0.exe"
REPL="$MONO ./repl.exe"
TDIR=../src/l/tests/

echo "L1"
$MBASE /I boot2.alc $TDIR/level1/test.al | grep -v OK
echo "L2: boot2.alc, libnet.alc, int"
$MBASE /I boot2.alc /I libnet.alc $TDIR/level2/test.al | grep -v OK
echo "L2: boot2.alc, libnet.alc, comp"
$MBASE /I boot2.alc /I libnet.alc $TDIR/level2/test_w.al | grep -v OK
echo "L2: boot2c.dll, libnet.alc, comp"
$MBASE /D boot2c.dll /I libnet.alc $TDIR/level2/test_w.al | grep -v OK
echo "L2: boot3.dll, comp"
$MBASE /D boot3.dll $TDIR/level2/test_w.al | grep -v OK
echo "L2: boot4.dll, comp"
$MBASE /D boot4.dll $TDIR/level2/test_w.al | grep -v OK
echo "L3: repl"
$REPL $TDIR/level3/test.al | grep -v OK
echo "L3: repl, exe comp"
$REPL /emit $TDIR/level3/test.al | grep -v OK
echo "L3: repl, exe run"
$MONO ./test.exe | grep -v OK

echo "L4: pfront"
$REPL $TDIR/level4/test.al | grep -v OK

