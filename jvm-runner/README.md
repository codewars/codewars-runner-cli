# JVM-runner

A codewars runner for JVM based languages.

## Building

To compile, first make sure you have JDK ≥ 1.8 and [leiningen](1) installed.  Then type:

	lein uberjar

This will create a JAR file `target/jvm-runner-*-standalone.jar` relative to the directory of this README.

## Usage

The runner can be run via `lein` or via the compiled JAR.

#### Clojure

- Using leiningen, no test fixture

      ```bash
      lein run <<< '{"language": "clojure", "solution": "(println \"42\")"}'
      ```

- Using JAR, no test fixture

      ```bash
      java -jar target/jvm-runner-*-standalone.jar <<< \
      '{"language": "clojure", "solution": "(println \"42\")"}'
      ```

#### Java

- Using leiningen, no test fixture


      ```bash
      lein run <<< '{
        "language": "java", 
        "solution": 
          "public class Solution { 
             public static void main() {
               System.out.println(\"42\");
             }
           }"}'
       ```

- Using JAR, no test fixture

      ```bash
      java -jar target/jvm-runner-*-standalone.jar <<< '{
        "language": "java", 
        "solution": 
          "public class Solution { 
             public static void main() {
               System.out.println(\"42\");
             }
           }"}'
      ```

#### Timeout

The JVM runner is programmed to exit after the code has run for a specified timeout.  Timeout defaults are controlled in the `:env` map in `project.clj`.  The default is set to *2000 msecs*.  It behaves as follows:

	lein run <<< '{"language": "clojure", "solution": "(Thread/sleep 5000) (println :ok)"}'
	# Exits with exit code 1 after ~2 seconds

The default can be overidden with an environement variable like so:

	TIMEOUT=10000 lein run <<< '{"language": "clojure", "solution": "(Thread/sleep 5000) (println :ok)"}'
	# Exits with exit code 0 and prints ":ok"

## Testing

Run:

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