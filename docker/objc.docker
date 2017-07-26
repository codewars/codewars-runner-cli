# EXAMPLE USAGE:  (IMAGE BUILD): docker build -t objc .
# EXAMPLE USAGE:  (Hello world): docker run objc run -l objc -c $'#import <Foundation/Foundation.h>\n\nint main (int argc, const char * argv[])\n{\n  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];\n  NSLog (@"Hello, World!");\n  [pool drain];\n  return 0;\n}'
# EXAMPLE USAGE:  (Modern objc): docker run objc run -l objc -c $'#import <Foundation/Foundation.h>\n\n// Modern Objective-C version\nNSNumber *ICKGetMaxProfit(NSArray<NSNumber *> *stockPricesYesterday, NSUInteger length) {\n    NSInteger minPrice, maxProfit;\n\n    // make sure we have at least 2 prices\n    NSCAssert(length >= 2,\n        [@"parameter length: expected 2 or more but got %lu", (unsigned long)length]);\n\n    // we\'ll greedily update minPrice and maxProfit, so we initialize\n    // them to the first price and the first possible profit\n    minPrice = stockPricesYesterday[0].integerValue;\n    maxProfit = stockPricesYesterday[1].integerValue - stockPricesYesterday[0].integerValue;\n\n    // start at the second (index 1) time\n    // we can\'t sell at the first time, since we must buy first,\n    // and we can\'t buy and sell at the same time!\n    // if we started at index 0, we\'d try to buy *and* sell at time 0.\n    // this would give a profit of 0, which is a problem if our\n    // maxProfit is supposed to be *negative*--we\'d return 0!\n    for (NSUInteger i = 1; i < length; i++) {\n        NSInteger currentPrice = stockPricesYesterday[i].integerValue;\n\n        // see what our profit would be if we bought at the\n        // min price and sold at the current price\n        NSInteger potentialProfit = currentPrice - minPrice;\n\n        // update maxProfit if we can do better\n        maxProfit = MAX(maxProfit, potentialProfit);\n\n        // update minPrice so it\'s always\n        // the lowest price we\'ve seen so far\n        minPrice = MIN(minPrice, currentPrice);\n    }\n\n    return @(maxProfit);\n}\n'

# Pull base image.
FROM codewars/base-runner

# Optionally mount '/objc-vol' on ephemeral storage vol for faster
# installing of packages, compiling, etc.
env OBJC_VOL /objc-vol
env OBJC_GEN_DIR $OBJC_VOL/objc-gen
env OBJC_SOURCE_DIR $OBJC_VOL/objc-source

RUN add-apt-repository ppa:ubuntu-toolchain-r/test
RUN add-apt-repository 'deb http://apt.llvm.org/trusty/ llvm-toolchain-trusty-3.9 main'
RUN wget -O - http://apt.llvm.org/llvm-snapshot.gpg.key| apt-key add -

RUN apt-key update
RUN apt-get update
RUN apt-get upgrade -y

# Install pre-requisities
RUN apt-get install -y cmake clang-3.9 lldb-3.9 llvm build-essential gobjc gobjc++ libblocksruntime-dev libkqueue-dev libpthread-workqueue-dev  libxml2-dev libjpeg-dev libtiff-dev libpng12-dev libcups2-dev libfreetype6-dev libcairo2-dev libxt-dev libgl1-mesa-dev libdispatch-dev
RUN apt-get install -y git libxslt1-dev libffi-dev libssl-dev libgnutls-dev libgmp3-dev libjpeg-dev libtiff-dev libpng-dev libgif-dev libx11-dev libcairo2-dev libxft-dev libxmu-dev libsqlite3-dev

RUN ln -s /usr/bin/clang-3.9 /usr/bin/clang
RUN ln -s /usr/bin/clang++-3.9 /usr/bin/clang++

# Set clang as compiler
ENV CC=clang
ENV CXX=clang++

#Install dependencies sources
RUN mkdir -p /source
WORKDIR /source

# Checkout sources
RUN mkdir -p icu && wget -q http://download.icu-project.org/files/icu4c/56.1/icu4c-56_1-src.tgz && tar -xf icu4c-56_1-src.tgz
RUN mkdir -p libobjc2 && wget -qO- https://github.com/gnustep/libobjc2/archive/v1.8.1.tar.gz | tar xz -C libobjc2 --strip-components=1
RUN mkdir -p make && wget -qO- ftp://ftp.gnustep.org/pub/gnustep/core/gnustep-make-2.6.7.tar.gz | tar xz -C make --strip-components=1
RUN mkdir -p base && wget -qO- ftp://ftp.gnustep.org/pub/gnustep/core/gnustep-base-1.24.9.tar.gz | tar xz -C base --strip-components=1
RUN mkdir -p gui && wget -qO- ftp://ftp.gnustep.org/pub/gnustep/core/gnustep-gui-0.25.0.tar.gz | tar xz -C gui --strip-components=1
RUN mkdir -p back && wget -qO- ftp://ftp.gnustep.org/pub/gnustep/core/gnustep-back-0.25.0.tar.gz | tar xz -C back --strip-components=1
RUN git clone https://github.com/nickhutchinson/libdispatch && cd libdispatch && git checkout bd1808980b04830cbbd79c959b8bc554085e38a1 && git clean -dfx
RUN git clone https://github.com/etoile/UnitKit && cd UnitKit && git clean -dfx

# Install ICU
WORKDIR /source/icu/source
RUN ./configure && make && make install
WORKDIR /source

# Install gnustep-makefile
WORKDIR /source/make
RUN ./configure --enable-debug-by-default --with-layout=gnustep --enable-objc-nonfragile-abi --enable-objc-arc
RUN make && make install
WORKDIR /source

# Install libobjc2
WORKDIR /source/libobjc2
RUN . /usr/GNUstep/System/Library/Makefiles/GNUstep.sh && cmake . -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_INSTALL_LIBDIR=/usr/lib -DCMAKE_ASM_COMPILER=clang -DTESTS=OFF
RUN . /usr/GNUstep/System/Library/Makefiles/GNUstep.sh && make -f Makefile
RUN . /usr/GNUstep/System/Library/Makefiles/GNUstep.sh && make -f Makefile install
WORKDIR /source

# Install libdispatch
WORKDIR /source/libdispatch
RUN ./configure && make && make install
WORKDIR /source

# add /usr/local/lib to libs search paths
RUN echo "/usr/local/lib" > /etc/ld.so.conf.d/my.conf
RUN ldconfig

ENV LDFLAGS="-L/usr/local/lib -ldispatch"
ENV OBJCFLAGS="-fblocks -fobjc-runtime=gnustep-1.8.1"

# Install gnustep-makefile again
WORKDIR /source/make
RUN . /usr/GNUstep/System/Library/Makefiles/GNUstep.sh && ./configure --enable-debug-by-default --with-layout=gnustep --enable-objc-nonfragile-abi --enable-objc-arc
RUN . /usr/GNUstep/System/Library/Makefiles/GNUstep.sh && make && make install
WORKDIR /source

# Install gnustep-base
WORKDIR /source/base
RUN . /usr/GNUstep/System/Library/Makefiles/GNUstep.sh && ./configure --disable-mixedabi && make && make install
WORKDIR /source

# Install gnustep-gui
WORKDIR /source/gui
RUN . /usr/GNUstep/System/Library/Makefiles/GNUstep.sh && ./configure && make && make install
WORKDIR /source

# Install gnustep-backend
WORKDIR /source/back
RUN . /usr/GNUstep/System/Library/Makefiles/GNUstep.sh && ./configure && make && make install
WORKDIR /source

# Install UnitKit
WORKDIR /source/UnitKit
RUN wget -q https://raw.githubusercontent.com/etoile/Etoile/master/etoile.make
RUN . /usr/GNUstep/System/Library/Makefiles/GNUstep.sh && make && make install
WORKDIR /

# Set environment variables
ENV USER codewarrior
ENV HOME /home/codewarrior
ENV PATH ${HOME}/GNUstep/Tools:/usr/GNUstep/Local/Tools:/usr/GNUstep/System/Tools:${PATH}
ENV LD_LIBRARY_PATH=${HOME}/GNUstep/Library/Libraries:/usr/GNUstep/Local/Library/Libraries:/usr/GNUstep/System/Library/Libraries:${LD_LIBRARY_PATH}
ENV GUILE_LOAD_PATH=${HOME}/GNUstep/Library/Libraries/Guile:/usr/GNUstep/Local/Library/Libraries/Guile:/usr/GNUstep/System/Library/Libraries/Guile
ENV GNUSTEP_IS_FLATTENED yes
ENV GNUSTEP_FLATTENED yes
ENV GNUSTEP_HOST x86_64-unknown-linux-gnu
ENV GNUSTEP_HOST_CPU x86_64
ENV GNUSTEP_MAKEFILES /usr/GNUstep/System/Library/Makefiles
ENV GNUSTEP_NETWORK_ROOT /usr/GNUstep/Network
ENV GNUSTEP_HOST_OS linux-gnu
ENV GNUSTEP_HOST_VENDOR unknown
ENV GNUSTEP_SYSTEM_ROOT /usr/GNUstep/System/Library
ENV GNUSTEP_LOCAL_ROOT /usr/GNUstep/Local/Library
ENV GNUSTEP_USER_ROOT ${HOME}/GNUstep
ENV OBJC_SOURCE_DIR /objc-vol/objc-source
ENV GNUSTEP_PATHLIST /usr/GNUstep/System:/usr/GNUstep/Network:/usr/GNUstep/Local:/root/GNUstep
ENV CLASSPATH  ${HOME}/GNUstep/Library/Libraries/Java:/usr/GNUstep/Local/Library/Libraries/Java:/usr/GNUstep/System/Library/Libraries/Java
ENV LIBRARY_COMBO=gnu-gnu-gnu
ENV INFOPATH=/usr/GNUstep/System/Library/Documentation/info::/usr/GNUstep/Local/Library/Documentation/info::${HOME}/GNUstep/Library/Documentation/info:

# Run UnitKit tests to make sure the environment works
WORKDIR /source/UnitKit
RUN make test=yes && ukrun -q TestSource/TestUnitKit/TestUnitKit.bundle


RUN ln -s /home/codewarrior /workspace
ENV NPM_CONFIG_LOGLEVEL warn

WORKDIR /runner
COPY package.json package.json
RUN npm install --production

COPY *.js ./
COPY lib/*.js lib/
COPY lib/*.sh lib/
COPY lib/utils lib/utils
COPY lib/runners/objc.js lib/runners/
COPY examples/objc.yml examples/
COPY test/runner.js test/
COPY test/runners/objc_spec.js test/runners/
COPY frameworks/objc /usr/local/include

USER codewarrior
ENV USER=codewarrior HOME=/home/codewarrior
RUN mocha -t 5000 test/runners/objc_spec.js

ENTRYPOINT ["node"]
