# http://www.kuznero.com/posts/haskell/building-statically-linked-binaries.html

# Please note: the combination of haskell:8.2.1 and
# ubuntu:17.10 is crucial for static linking against net foo
# otherwise: Network.BSD.getProtocolByName: does not exist (no such protocol name: tcp) in haskell

FROM haskell:8.2.1 as builder
COPY . /src
WORKDIR /src
RUN cabal update && \
    cabal install --dependencies-only --force-reinstalls
RUN cabal configure --disable-executable-dynamic --ghc-option=-optl=-static --ghc-option=-optl=-pthread
RUN cabal build
RUN mkdir /out
RUN cp -v dist/build/grendel-exe/grendel-exe /out

FROM ubuntu:17.10
RUN apt-get update && \
    apt-get install -y ca-certificates && \
    apt-get install -y libgnutls30 && \
    apt-get install -y netbase && \
    apt-get -y autoremove && \
    apt-get -y clean && \
    rm -rf /var/lib/apt/lists/* && \
    rm -rf /tmp/* && \
    rm -rf /var/tmp/*
COPY --from=builder /out/grendel-exe /
CMD ["/grendel-exe"]
