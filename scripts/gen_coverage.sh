#!/usr/bin/env bash

set -euo pipefail

fpm test --profile debug --flag --coverage --flag -O0

rm -rf coverage_html
mkdir -p coverage_html

mapfile -t gcda_files < <(find build -type f -path '*/DepAC/src_*.gcda' -print)
if (( ${#gcda_files[@]} == 0 )); then
  echo "ERROR: no DepAC coverage data files found" >&2
  exit 1
fi

gcov "${gcda_files[@]}" -b
shopt -s nullglob
gcov_files=(*.f90.gcov)
if (( ${#gcov_files[@]} == 0 )); then
  echo "ERROR: gcov did not generate Fortran coverage files" >&2
  exit 1
fi
mv "${gcov_files[@]}" coverage_html/

geninfo "${gcda_files[@]}" -b . -o coverage_html/coverage.info
genhtml coverage_html/coverage.info -o coverage_html/temp