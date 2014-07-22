# BUILD-USING:    docker build -t codewars/jvm-runner .
# EXAMPLE USAGE:  docker run -i -a stdin -a stdout -a stderr --rm codewars/jvm-runner <<< '{"language": "clojure", "solution": "(println 42)"}'

# Pull base image.
FROM dockerfile/ubuntu

# Install Java 8
# RUN apt-get install -y default-jre-headless default-jdk # default is OpenJDK6
RUN add-apt-repository ppa:webupd8team/java 
RUN apt-get update
# http://askubuntu.com/a/190674
RUN echo debconf shared/accepted-oracle-license-v1-1 select true | debconf-set-selections && \
    echo debconf shared/accepted-oracle-license-v1-1 seen true | debconf-set-selections
RUN apt-get install -y oracle-java8-installer

# Install Leiningen
RUN curl https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein > /usr/bin/lein
RUN chmod a+x /usr/bin/lein
RUN LEIN_ROOT=true lein --version

# ADD codewarrior user
RUN useradd -s /usr/sbin/nologin codewarrior

# ADD cli-runner and install node deps
ADD . /codewars-jvm
WORKDIR /codewars-jvm
RUN LEIN_ROOT=true lein do clean, test, uberjar

# Set the user and workdir
WORKDIR /tmp
USER codewarrior

# Run the uberjar
# TODO: Infer the version number for this somehow
ENV TIMEOUT 2000
ENTRYPOINT ["java", "-jar", "/codewars-jvm/target/jvm-runner-0.1.1-standalone.jar"]
