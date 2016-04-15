#!/bin/bash

sed -e "1,/^Test Suite '${1}.xctest' started at/d" \
    -e "s/\(^Test Suite '[^']*' started at .*\)$/<DESCRIBE::>\1/" \
    -e "s/\(^Test Case '[^']*' started.\)$/<IT::>\1/" \
    -e "s/\(^Test Case '[^']*' failed [^(]*\)(\([^)]*\))\.$/<FAILED::>\1<COMPLETEDIN::>\2/" \
    -e "s/\(^Test Case '[^']*' passed [^(]*\)(\([^)]*\))\.$/<PASSED::>\1<COMPLETEDIN::>\2/" \
    -e $'s/<COMPLETEDIN::>/\\\n<COMPLETEDIN::>/g' \
    -e "s/${1//-/_}\.//g" \
    -e "s/^.*\/${1}\/\([^\.]*\.swift\)/\1/" \
    -e "s/^.*\/${1}\/\([^\.]*\.m\)/\1/" \
    -e "/^Test Suite '${1}.xctest' failed/,\$d" \
    -e "/^Test Suite '${1}.xctest' passed/,\$d" |
perl -pe "s/(Test Suite '[^']*' [pf]a[is][ls]ed.*)\n/<COMPLETEDIN::>\1<:LF:>/"
