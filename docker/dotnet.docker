# BUILD-USING:    docker build -t codewars/runner-dotnet .
# TEST-USING:     docker run --rm -i -t --name=test-runner-dotnet --entrypoint=/bin/bash codewars/runner-dotnet -s
# RUN-USING:      docker run --rm --name=runner-dotnet codewars/runner-dotnet --help

# Pull base image.
FROM codewars/base-runner

RUN echo "Forcing Image Rebuild"
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
RUN echo "deb http://download.mono-project.com/repo/debian wheezy main" | sudo tee /etc/apt/sources.list.d/mono-xamarin.list
RUN echo "deb http://download.mono-project.com/repo/debian wheezy-apache24-compat main" | sudo tee -a /etc/apt/sources.list.d/mono-xamarin.list

RUN apt-get update

# Install Mono
RUN apt-get install -y mono-complete mono-xsp4 referenceassemblies-pcl

# Install F#
RUN apt-get install -y fsharp

# Install .NET Core SDK
# TODO: add back in once support is completed, right now we are sticking with Mono only
RUN sh -c 'echo "deb [arch=amd64] http://apt-mo.trafficmanager.net/repos/dotnet-release/ trusty main" > /etc/apt/sources.list.d/dotnetdev.list'
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 417A0893
RUN apt-get update

RUN apt-get install -y dotnet-dev-1.0.0-preview2-1-003177

RUN apt-get install -y nuget

# RUN mkdir /netcore
# ADD frameworks/csharp/netcore/WebChallenge.csproj /netcore/project.csproj
# RUN (cd /netcore && dotnet restore)


RUN ln -s /home/codewarrior /workspace
ENV NPM_CONFIG_LOGLEVEL warn

WORKDIR /runner
COPY package.json package.json
RUN npm install --production

COPY *.js ./
COPY lib/*.js lib/
COPY lib/*.sh lib/
COPY lib/utils lib/utils
COPY lib/runners/csharp.js lib/runners/
COPY lib/runners/fsharp.js lib/runners/
COPY examples/csharp.yml examples/
COPY examples/fsharp.yml examples/
COPY frameworks/csharp frameworks/csharp
COPY frameworks/fsharp frameworks/fsharp
COPY test/runner.js test/
COPY test/runners/csharp_spec.js test/runners/
COPY test/runners/fsharp_spec.js test/runners/

USER codewarrior
ENV USER=codewarrior HOME=/home/codewarrior

# RUN mocha -t 10000 test/runners/csharp_spec.js
RUN mocha -t 10000 test/runners/fsharp_spec.js
RUN sh /runner/lib/cleanup.sh
RUN mono -V

ENTRYPOINT ["node"]
