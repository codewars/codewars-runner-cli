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
      

## License

Copyright © 2014 Matthew Wampler-Doty

[1]: http://leiningen.org/
