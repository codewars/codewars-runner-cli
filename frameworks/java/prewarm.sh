#!/bin/bash

echo "loading" > /workspace/prewarm.status

# prewarm by starting the gradle daemon. Running an initial test build will also speed things up a bit
cd /runner/frameworks/java && gradle --daemon --offline test

echo "loaded" > /workspace/prewarm.status

# node run -l java -c "public class Solution {}" -f "import org.junit.Test;public class TestFixture{}"

