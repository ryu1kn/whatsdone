#!/bin/bash

set -euo pipefail

echo Commit range: $TRAVIS_COMMIT_RANGE

echo List of changed file(s) in the range:
git diff --name-only $TRAVIS_COMMIT_RANGE

git diff --name-only $TRAVIS_COMMIT_RANGE | BUILD_NUMBER=$TRAVIS_BUILD_NUMBER ./node_modules/.bin/buildman
