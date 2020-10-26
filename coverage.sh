#!/bin/env sh

fns=$(grep "^(fn" core.fnl macros/core.fnl | cut -f2 --delimiter=" " | grep -v "^-")
tsts=$(for fn in $fns; do fn=$(echo "$fn" | sed "s/\?/\\\?/"); grep "(test $fn" core_test.fnl macros_test.fnl | cut -f2 --delimiter=" "; done)
not_tested=$(printf "%s\n%s" "$fns" "$tsts" | sort | uniq -u)

total_fns=$(echo "$fns" | wc -l)
total_not_tested=$(echo "$not_tested" | wc -l)
coverage=$((100 - (("$total_not_tested" * 100) / "$total_fns")))

echo "coverage: $coverage%" >&2
echo "not tested functions:" >&2
echo "$not_tested" | tr '\n' ' ' >&2
echo >&2
