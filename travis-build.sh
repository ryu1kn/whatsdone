#!/bin/bash

set -euo pipefail

git diff --name-only $TRAVIS_COMMIT_RANGE | BUILD_NUMBER=$TRAVIS_BUILD_NUMBER ./node_modules/.bin/buildman
