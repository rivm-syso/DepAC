#!/usr/bin/env bash

set -euo pipefail

README_FILE="README.md"
COVERAGE_INFO_FILE="coverage_html/coverage.info"
VERSION_MODULE_FILE="src/m_version.f90"
ENV_OUTPUT_FILE="${COVERAGE_ENV_FILE:-/tmp/depac_coverage.env}"

for required_file in "${README_FILE}" "${COVERAGE_INFO_FILE}" "${VERSION_MODULE_FILE}"; do
  if [[ ! -f "${required_file}" ]]; then
    echo "ERROR: ${required_file} not found" >&2
    exit 1
  fi
done

coverage_value="$(awk -F: '
  /^LF:/ { lf += $2 }
  /^LH:/ { lh += $2 }
  END {
    if (lf > 0) {
      printf "%.2f", (100.0 * lh / lf)
    } else {
      printf "0.00"
    }
  }
' "${COVERAGE_INFO_FILE}")"

if [[ "${coverage_value}" == "100.00" ]]; then
  badge_color="green"
elif [[ "${coverage_value}" == "0.00" ]]; then
  badge_color="red"
else
  badge_color="orange"
fi

coverage_badge="![Coverage](https://img.shields.io/badge/coverage-${coverage_value}%25-${badge_color})"
build_date="$(sed -n 's/.*BUILD_DATE = "\([0-9-]\+\)".*/\1/p' "${VERSION_MODULE_FILE}" | head -n 1)"
if [[ -z "${build_date}" ]]; then
  echo "ERROR: could not determine BUILD_DATE" >&2
  exit 1
fi

release_date_badge="${build_date//-/--}"
release_badge="![Release Date](https://img.shields.io/badge/release-${release_date_badge}-blue)"

if grep -q '^!\[Coverage\](https://img.shields.io/badge/coverage-' "${README_FILE}"; then
  sed -i "s|^!\[Coverage\](https://img.shields.io/badge/coverage-.*)|${coverage_badge}|" "${README_FILE}"
elif grep -q '^!\[Static Badge\](https://img.shields.io/badge/coverage-' "${README_FILE}"; then
  sed -i "s|^!\[Static Badge\](https://img.shields.io/badge/coverage-.*)|${coverage_badge}|" "${README_FILE}"
else
  sed -i "1a ${coverage_badge}" "${README_FILE}"
fi

if grep -q '^!\[Release Date\](https://img.shields.io/badge/release-' "${README_FILE}"; then
  sed -i "s|^!\[Release Date\](https://img.shields.io/badge/release-.*)|${release_badge}|" "${README_FILE}"
else
  printf '\n%s\n' "${release_badge}" >> "${README_FILE}"
fi

printf 'COVERAGE_PERCENT=%s\nCOVERAGE_COLOR=%s\nBUILD_DATE=%s\n' \
  "${coverage_value}" "${badge_color}" "${build_date}" > "${ENV_OUTPUT_FILE}"

echo "Computed coverage: ${coverage_value}%"
echo "Badge color: ${badge_color}"
echo "Build date: ${build_date}"
echo "Coverage env file: ${ENV_OUTPUT_FILE}"