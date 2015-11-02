#!/bin/bash

if [[ $# -lt 1 ]]; then
  echo tag
  exit 22
fi

git tag -d "$1"
git push origin ":refs/tags/$1"
