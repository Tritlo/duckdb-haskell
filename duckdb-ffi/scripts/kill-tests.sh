#!/bin/bash

while true; do
    echo "Running:"
    ps -e | grep duckdb
    echo "Killing."
    ps -e | grep duckdb | awk '{print $1}' | xargs -r kill -9
    echo "Sleeping for $1 seconds..."
    sleep $1
done