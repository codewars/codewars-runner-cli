# JVM-runner

A codewars runner for JVM based languages. The advantage of using this runner is that it handles warming up the JVM for
faster execution times.

## Building

To compile, first make sure you have JDK ≥ 1.8 and [leiningen][1] installed.  Then type:

	lein uberjar

This will create a JAR file `target/jvm-runner-*-standalone.jar` relative to the directory of this README.

## Usage

The runner can be run via `lein` or via the compiled JAR.

By design, the JVM runner accepts JSONs with the same API as the node.js runner.  These are handed as strings in _stdin_.

JSONs should conform to the following [JSON schema][2]:

```json
{
    "title": "CodeWars Runner Schema",
    "type": "object",
    "properties": {
        "language": {
                "description": "The language the kata was written in",
                "type": "string"
        },
        "solution": {
            "description": "The codewars kata solution code",
            "type": "string"
        },
        "fixture": {
            "description": "The test suite for the solution code",
            "type": "string"
        },
        "setup": {
            "description": "Additional setup code",
            "type": "string"
        }
    },
    "required": ["language", "solution"]
}
```

### Three Ways of Running the Runner

1. Use `lein run`:

```bash
lein run <<< '{"language": "clojure", "solution": "(println 42)"}'
```
      
2. Build an *uberjar* and run that with java:

```bash
lein do clean, uberjar
TIMEOUT=2000 java -jar target/jvm-runner-*-standalone.jar <<< \
'{"language": "clojure", "solution": "(println 42)"}'
```

3. Build the Docker container, and run that:

```bash
docker build -t codewars/jvm-runner .
docker run -i -a stdin -a stdout -a stderr --rm codewars/jvm-runner <<< \
'{"language": "clojure", "solution": "(println \"42\")"}'
```

#### Clojure

Examples:

- No test fixture

```bash
lein run <<< '{"language": "clojure", "solution": "(println 42)"}'
```

- Simple test fixture

```bash
java -jar target/jvm-runner-*-standalone.jar <<< \
'{"language": "clojure", 
"solution": "(ns a) (defn b [] 1)", 
"fixture": "(ns test (:use [a] [clojure.test])) 
            (deftest b-test (is (= 1 (b))))"}'
```

#### Java

- No test fixture

```bash
lein run <<< '{
  "language": "java", 
  "solution": "public class Solution { 
                 public static void main() {
                   System.out.println(\"42\");}}"}'
```

#### Timeout

The JVM runner is programmed to exit after the code has run for a specified timeout.  Timeout defaults are controlled in the `:env` map in `project.clj`.  The default is set to *2000 msecs*.  It behaves as follows:

	lein run <<< '{"language": "clojure", "solution": "(Thread/sleep 5000) (println :ok)"}'
	# Exits with exit code 1 after ~2 seconds

The default can be overidden with an environement variable:

	TIMEOUT=10000 lein run <<< '{"language": "clojure", "solution": "(Thread/sleep 5000) (println :ok)"}'
	# Exits with exit code 0 and prints ":ok"
	
Note that if you are not in this directory and running the *uberjar*, you will have to set the `TIMEOUT` environment variable manually.

## Testing

	lein test

## Development

### Clojure

For clojure, popular choices of IDE include Emacs, Vi, and LightTable.  All of these, when correctly configured, should auto-detect the `project.clj` file and intelligently pull artifacts necessary for running and testing as necessary.

### Java

It is often desirable to use an IDE like Eclipse or IntelliJ for Java development.  To do this, you will want to generate a maven pom file.  This is done using leiningen as follows:

	lein pom

This will generate a `pom.xml` file suitable for use with maven and other tools that plug into maven.


## License

Copyright © 2014 Matthew Wampler-Doty

[1]: http://leiningen.org/
[2]: http://json-schema.org/
