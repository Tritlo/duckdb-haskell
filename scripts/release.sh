#!/bin/bash


rm -rf dist-newstyle/*-docs.tar.gz
rm -rf dist-newstyle/sdist/*
cabal haddock $1 --haddock-for-hackage
cabal sdist $1
read -p "Username: " username
read -sp "Password: " password

cabal upload --publish -u "$username" -p "$password" dist-newstyle/sdist/$1-*.tar.gz
cabal upload --publish -d -u "$username" -p "$password" dist-newstyle/$1-*-docs.tar.gz