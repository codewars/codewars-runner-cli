# Environment

Code is executed within a Dockerized Ubuntu 14.04 container. 

## Languages

- Java 8 (1.8.0_91)

## Loaded Dependencies

### The following depencies are always loaded

- junit 4.12
- lombok 1.16.18
- mockito-core 2.7.19
- assertj-core 3.8.0

### The following can be loaded through `@config reference` statements

- joda-time 2.2
- guava 20.0
- commons-lang3 3.6
- commons-math3 3.6.1
- jsoup 1.10.3
- dom4j 2.0.1
- assertj-guava 3.1.0
- hibernate-core 5.2.10.Final
- mongo-java-driver 3.4.2
- sqlite-jdbc 3.19.3
- postgresql 42.1.1
- spring-boot-starter-web 1.5.4
- spring-boot-starter-test 1.5.4
- spring-boot-starter-data-mongodb 1.5.4
- spring-boot-starter-data-redis 1.5.4
- spring-boot-starter-data-jpa 1.5.4
- spring-boot-starter-data-rest 1.5.4
- spring-boot-starter-validation 1.5.4


To make these packages available to the application, you must have access to the setup code block. 
Within the setup code you can load any of these packages using reference config statements. 

**Setup Example:**
```java
// @config: reference guava
// @config: reference commons-lang3
```

 
If you need to reference a package that is a dependency of one of the above packages, you will need to load those packages
in order to make that dependency available.

### Spring Boot Packages

If you require support for the Spring framework, you can include `spring-boot` as the reference name. 
This will include both the web and test starter dependencies, as well as any additional requirements. 
 
When including the Spring framework via `spring-boot`, if other services are configured, such as mongodb, then the required spring data packages will also be auto-included into the build.

# Build Process

Gradle is used as the build tool. Each time you run code, a fresh Docker container will be used. Under
typical conditions the Gradle daemon should have already loaded, causing build times to typically fall within
the 3 to 4 second range for trivial sized apps. However if the daemon has not finished loading then the build process
will need to wait until the daemon is ready. This should not affect output walltime but may cause delays in code execution.

# Timeout

The sandbox environment will timeout the code within 20 seconds. 

> For more information, view the [docker file](https://github.com/Codewars/codewars-runner-cli/blob/master/docker/jvm.docker) 
