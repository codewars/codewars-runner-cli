## EXAMPLE USAGE:  docker run --rm codewars/python-runner run -l python -c "print 1+1 "
##                 docker run --rm codewars/python-runner run -l python3 -c "print(1+1)"
#
## Pull base image.
#FROM codewars/base-runner
#
## Install Additional Python libraries
#RUN apt-get install -y python-pip python-numpy python-scipy python-pandas
#
## Install Python 3
#RUN apt-get install -y python3-pip python3-numpy python3-scipy python3-pandas
#
## Install Packages
#RUN pip install pymongo redis
#RUN pip3 install pymongo redis
#
## Install TensorFlow for deep learning capabilities (CPU support only)
#RUN pip install --upgrade https://storage.googleapis.com/tensorflow/linux/cpu/tensorflow-0.8.0rc0-cp27-none-linux_x86_64.whl
#RUN pip3 install --upgrade https://storage.googleapis.com/tensorflow/linux/cpu/tensorflow-0.8.0rc0-cp34-cp34m-linux_x86_64.whl
#
## Install Scikit-learn
#RUN pip install -U scikit-learn
#RUN pip3 install -U scikit-learn
#
## Install Tornado
#RUN pip install -U tornado
#RUN pip3 install -U tornado
#
## Install Ably
#RUN pip install -U ably
#RUN pip3 install -U ably
#
## Install Django
#ENV DJANGO_VERSION 1.9.8
#RUN pip install -U psycopg2 django=="$DJANGO_VERSION"
#RUN pip3 install -U psycopg2 django=="$DJANGO_VERSION"
#
## Install PySVG
#RUN pip install -U pysvg
#RUN pip3 install -U pysvg
#
## Install mpld3 package for rendering images to browser
#RUN pip install mpld3
#RUN pip3 install mpld3
#
#RUN pip install Jinja Jinja2
#RUN pip3 install Jinja2
#
## Install PyCrypto
#RUN pip install -U pycrypto
#RUN pip3 install -U pycrypto
#
## Install gmpy2 for Python2/Python3
#RUN apt-get install -y python-gmpy2 python3-gmpy2
#
## add the package json first to a tmp directory and build, copy over so that we dont rebuild every time
#ADD package.json /tmp/package.json
#RUN cd /tmp && npm install --production
#RUN mkdir -p /runner && cp -a /tmp/node_modules /runner
#
#RUN ln -s /home/codewarrior /workspace
#
## ADD cli-runner and install node deps
#ADD . /runner
#
#WORKDIR /runner
#
## Run the test suite to make sure this thing works
#
#USER codewarrior
## Set environment variables
#
#ENV USER codewarrior
#ENV HOME /home/codewarrior
#RUN mocha -t 5000 test/runners/python_spec.js
#
##timeout is a fallback in case an error with node
##prevents it from exiting properly
#ENTRYPOINT ["timeout", "15", "node"]

# Pull base image.
FROM codewars/base-runner

# Optionally mount '/objc-vol' on ephemeral storage vol for faster
# installing of packages, compiling, etc.
env OBJC_VOL /objc-vol
env OBJC_GEN_DIR $OBJC_VOL/objc-gen
env OBJC_SOURCE_DIR $OBJC_VOL/objc-source

# NEW
RUN rm /etc/apt/sources.list
RUN echo "deb http://archive.ubuntu.com/ubuntu/ xenial main restricted" >> /etc/apt/sources.list
RUN echo "deb-src http://archive.ubuntu.com/ubuntu/ xenial main restricted" >> /etc/apt/sources.list
RUN echo "deb http://archive.ubuntu.com/ubuntu/ xenial-updates main restricted" >> /etc/apt/sources.list
RUN echo "deb-src http://archive.ubuntu.com/ubuntu/ xenial-updates main restricted" >> /etc/apt/sources.list
RUN echo "deb http://archive.ubuntu.com/ubuntu/ xenial universe" >> /etc/apt/sources.list
RUN echo "deb-src http://archive.ubuntu.com/ubuntu/ xenial universe" >> /etc/apt/sources.list
RUN echo "deb http://archive.ubuntu.com/ubuntu/ xenial-updates universe" >> /etc/apt/sources.list
RUN echo "deb-src http://archive.ubuntu.com/ubuntu/ xenial-updates universe" >> /etc/apt/sources.list
RUN echo "deb http://archive.ubuntu.com/ubuntu/ xenial-security main restricted" >> /etc/apt/sources.list
RUN echo "deb-src http://archive.ubuntu.com/ubuntu/ xenial-security main restricted" >> /etc/apt/sources.list
RUN echo "deb http://archive.ubuntu.com/ubuntu/ xenial-security universe" >> /etc/apt/sources.list
RUN echo "deb-src http://archive.ubuntu.com/ubuntu/ xenial-security universe" >> /etc/apt/sources.list

#RUN cat /etc/apt/sources.list
#RUN sudo sed -i '/canonical\|extras/s/^/#/;/^[^#]/s|//[^/]*|//us.ubuntu.com|'  /etc/apt/sources.list

# sudo apt-get install update-manager-core
# do-release-upgrade -y

#RUN cat /etc/apt/sources.list
#RUN sudo rm -vf /var/lib/apt/lists/*
RUN apt-get update
RUN apt-get upgrade -y
#RUN sudo apt-get dist-upgrade -y
#RUN add-apt-repository main
#RUN add-apt-repository universe
#RUN add-apt-repository restricted
#RUN add-apt-repository multiverse

#RUN cat <<EOT >> /etc/apt/apt.conf.d/99fixbadproxy
#RUN Acquire::http::Pipeline-Depth "0";
#RUN Acquire::http::No-Cache=True;
#RUN Acquire::BrokenProxy=true;
#RUN EOT
#RUN sudo rm  /var/lib/apt/lists/*
#RUN sudo rm  /var/lib/apt/lists/partial/*
#RUN sudo apt-get update

RUN apt-get install -y build-essential gobjc gobjc++ gnustep gnustep-devel libgnustep-base-dev clang gnustep-make llvm libblocksruntime-dev

# Install libobjc
ADD headers/objc/objc /usr/include/GNUstep/objc
ADD headers/objc/CW /usr/include/GNUstep/CW

#RUN chmod -R a+rx OBJC_VOL/*

# Add the package json first to a tmp directory and build, copy over so that we dont rebuild every time
ADD package.json /tmp/package.json
RUN cd /tmp && npm install --production
RUN mkdir -p /runner && cp -a /tmp/node_modules /runner

# ADD cli-runner and install node deps
ADD . /runner

WORKDIR /runner

# Run the test suite to make sure this thing works

USER codewarrior

# Set environment variables
ENV USER codewarrior
ENV HOME /home/codewarrior
RUN ls -la
RUN mocha -t 5000 test/runners/objc_spec.js

# timeout is a fallback in case an error with node
# prevents it from exiting properly
ENTRYPOINT ["timeout", "15", "node"]
