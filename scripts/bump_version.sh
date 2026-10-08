#!/usr/bin/env bash

set -euo pipefail

VERSION_FILE="VERSION"
VERSION_MODULE_FILE="src/m_version.f90"

for required_file in "${VERSION_FILE}" "${VERSION_MODULE_FILE}"; do
  if [[ ! -f "${required_file}" ]]; then
    echo "ERROR: ${required_file} not found" >&2
    exit 1
  fi
done

current_version="$(tr -d '[:space:]' < "${VERSION_FILE}")"
if [[ -z "${current_version}" ]]; then
  current_version="0.0.0"
fi

normalized_current="${current_version#v}"

is_semver() {
  [[ "$1" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]
}

latest_tag_version="$(git tag --list | sed 's/^v//' | grep -E '^[0-9]+\.[0-9]+\.[0-9]+$' | sort -V | tail -n 1 || true)"

main_version=""
if git rev-parse --verify origin/main >/dev/null 2>&1; then
  main_version="$(git show origin/main:VERSION 2>/dev/null | tr -d '[:space:]' | sed 's/^v//' || true)"
fi

version_candidates="${normalized_current}"
if is_semver "${latest_tag_version:-0.0.0}"; then
  version_candidates+=$'\n'"${latest_tag_version}"
fi
if is_semver "${main_version:-0.0.0}"; then
  version_candidates+=$'\n'"${main_version}"
fi

baseline_version="$(printf '%s\n' "${version_candidates}" | grep -E '^[0-9]+\.[0-9]+\.[0-9]+$' | sort -V | tail -n 1)"
IFS='.' read -r major minor patch extra <<< "${baseline_version}"

if [[ -n "${extra:-}" ]] || [[ -z "${major:-}" ]] || [[ -z "${minor:-}" ]] || [[ -z "${patch:-}" ]]; then
  echo "ERROR: VERSION must use semantic format MAJOR.MINOR.PATCH (optionally prefixed with v)" >&2
  exit 1
fi

commit_message="$(git log -1 --pretty=%B)"
if [[ "${commit_message}" == *"[major]"* ]]; then
  major=$((major + 1))
  minor=0
  patch=0
elif [[ "${commit_message}" == *"[minor]"* ]]; then
  minor=$((minor + 1))
  patch=0
else
  patch=$((patch + 1))
fi

new_version="${major}.${minor}.${patch}"
build_date="$(date -u +%Y-%m-%d)"

printf '%s\n' "${new_version}" > "${VERSION_FILE}"
sed -i -E "s/(character\(len=\*\), parameter :: VERSION = \”).*(\”)/\1${new_version}\2/" "${VERSION_MODULE_FILE}"
sed -i -E "s/(character\(len=\*\), parameter :: BUILD_DATE = \”).*(\”)/\1${build_date}\2/" "${VERSION_MODULE_FILE}"

cat > version.env <<EOF
OLD_VERSION=${normalized_current}
NEW_VERSION=${new_version}
BUILD_DATE=${build_date}
BASELINE_VERSION=${baseline_version}
EOF

echo "Bumped VERSION: ${normalized_current} -> ${new_version} (baseline: ${baseline_version})"
echo "Updated BUILD_DATE: ${build_date}"