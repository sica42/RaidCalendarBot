#!/usr/bin/env bash

function main() {
  if [[ $# -ne 1 ]]; then
    echo "Usage: $0 <tag>"
    exit 1
  fi

  NEW_VERSION=$1

  git tag "v$NEW_VERSION" -f
  git push origin "v$NEW_VERSION" -f

  # Update pom.xml
  sed -i -E "0,/<version>[0-9]+\.[0-9]+\.[0-9]+<\/version>/s//<version>$NEW_VERSION<\/version>/" pom.xml

  # Update README.md
  sed -i -E "s|raidcalendar-[0-9]+\.[0-9]+\.[0-9]+\.zip|raidcalendar-$NEW_VERSION.zip|g" README.md

  # Update RaidCalendar.scala
  sed -i -E "s|private val RELEASE = \"v[0-9]+\.[0-9]+\.[0-9]+\"|private val RELEASE = \"v$NEW_VERSION\"|g" src/main/scala/raidcalbot/RaidCalendar.scala

  # Append new changelog entry
  printf "\n%s - v%s\n" "$(date +%Y.%m.%d)" "$NEW_VERSION" >> CHANGELOG

  echo "Version updated successfully!"
}

main "$@"
