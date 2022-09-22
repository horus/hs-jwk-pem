FROM haskell:9.2.4-buster as builder
# -fllvm, 13)
RUN apt update && apt install -y llvm-11
RUN update-alternatives --install /usr/bin/opt opt /usr/bin/opt-11 100
RUN update-alternatives --install /usr/bin/llc llc /usr/bin/llc-11 100
RUN mkdir -p hs-jwk-pem
COPY . hs-jwk-pem
WORKDIR /hs-jwk-pem
RUN cabal update && cabal build && cabal install --enable-executable-stripping --install-method=copy --overwrite-policy=always --installdir=/tmp
RUN strip --strip-unneeded /tmp/hs-jwk-pem-exe

FROM debian:buster-slim
COPY --from=builder /tmp/hs-jwk-pem-exe /
ENTRYPOINT ["/hs-jwk-pem-exe", "web"]
