#!/usr/bin/env bash

## Stop on errors:
set -eo pipefail

## Helper function to print log messages.
_log() {
    echo "[RELEASE LOG]" "${@}"
}

## Helper function to print errors.
_error() {
    1>&2 echo "[RELEASE ERROR]" "${@}"
}

## Helper function to print usage.
_usage() {
    echo "Usage: $0 [-h] -n <VERSION>"
}

## Declare variables:
_appname="habulara"
_version=""
_infile="result/bin/${_appname}"
_outfile=""

## Parse command line arguments:
while getopts "n:h" o; do
    case "${o}" in
    n)
        _version="${OPTARG}"
        _outfile="${_appname}-v${_version}-$(uname -s)-$(uname -m)-static"
        ;;
    h)
        _usage
        exit 0
        ;;
    *)
        1>&2 _usage
        exit 1
        ;;
    esac
done
shift $((OPTIND - 1))

_log "Checking variables..."
if [ -z "${_version}" ]; then
    1>&2 _usage
    exit 1
else
    _log "Version is ${_version}. Proceeding..."
    _log "Statically built, compressed executable binary filename is ${_outfile}. Proceeding..."
fi

_log "Checking repository..."
if [[ -z "$(git status --porcelain)" ]]; then
    _log "Repository is clean. Proceeding..."
else
    _error "Repository is not clean. Aborting..."
    exit 1
fi

_log "Updating application version..."
sed -i -E "s/^version:([ ]+).*/version:\\1${_version}/g" package.yaml
hpack

_log "Generating changelog..."
git-chglog --output CHANGELOG.md --next-tag "${_version}"

_log "Staging changes..."
git add "${_appname}.cabal" package.yaml CHANGELOG.md

_log "Committing changes..."
git commit -m "chore(release): v${_version}"

_log "Tagging version..."
git tag -a -m "Release v${_version}" "${_version}"

_log "Building static binary..."
nix-build --arg doStatic true

_log "Compressing static binary..."
upx -o "${_outfile}" "${_infile}"

_log "Pushing changes to remote..."
git push --follow-tags origin main

_log "Creating the release..."
gh release create "${_version}" --generate-notes

_log "Uploading release artifacts..."
gh release upload "${_version}" "${_outfile}"

_log "Removing compressed, static binary..."
rm -f "${_outfile}"

_log "Finished!"
