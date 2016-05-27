# Environment

Code is executed within a Dockerized Ubuntu 14.04 container. 

## Languages

### C# 6.0

Mono JIT compiler version 4.2.3 installed via mono-complete package

All code is compiled with the following libraries:

- nunit
- System.Numerics
- System.Drawing
- System.Data
- System.Messaging
- System.Xml


### F# 4.0

F# currently is not loaded with any libraries and does not have test integration support

> For more information, view the [docker file](https://github.com/Codewars/codewars-runner-cli/blob/master/docker/dotnet.docker)
