#!/bin/bash

echo "loading" > /workspace/prewarm.status

# prewarm by starting the gradle daemon. Running an initial test build will also speed things up a bit
cd /runner/frameworks/gradle && gradle --daemon --offline test

echo "loaded" > /workspace/prewarm.status

echo "prewarmed"
