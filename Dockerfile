FROM haskell:9.2.3-slim
# -fllvm, 13)
RUN apt update && apt install -y llvm-11
RUN update-alternatives --install /usr/bin/opt opt /usr/bin/opt-11 100
RUN update-alternatives --install /usr/bin/llc llc /usr/bin/llc-11 100
RUN mkdir -p hs-jwk-pem
COPY . hs-jwk-pem
WORKDIR hs-jwk-pem
RUN stack upgrade
RUN stack update
RUN stack build
CMD stack run -- web
