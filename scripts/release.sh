#!/bin/bash


rm -rf dist-newstyle/*-docs.tar.gz
rm -rf dist-newstyle/sdist/*
cabal haddock all --haddock-for-hackage
cabal sdist all
read -p "Username: " username
read -sp "Password: " password

cabal upload --publish -u "$username" -p "$password" dist-newstyle/sdist/duckdb-ffi-*.tar.gz
cabal upload --publish -d -u "$username" -p "$password" dist-newstyle/duckdb-ffi-*-docs.tar.gz
cabal upload --publish -u "$username" -p "$password" dist-newstyle/sdist/duckdb-simple-*.tar.gz
cabal upload --publish -d -u "$username" -p "$password" dist-newstyle/duckdb-simple-*-docs.tar.gz