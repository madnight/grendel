# http://www.kuznero.com/posts/haskell/building-statically-linked-binaries.html

# Please note: the combination of haskell:8.2.1 and
# debian:9.4-slim is crucial for static linking against net foo
# otherwise: Network.BSD.getProtocolByName:
# does not exist (no such protocol name: tcp) in haskell

# cache image that contains the cabal dependencies
# it is significantly faster to copy them from another image than cabal install
FROM madnight/grendel:cache as cache

FROM haskell:8.2.1 as builder
COPY --from=cache / /
COPY . /src
WORKDIR /src
RUN cabal update && \
    cabal install -j \
    --dependencies-only
RUN cabal configure \
    --disable-executable-dynamic \
    --ghc-option=-optl=-static \
    --ghc-option=-optl=-pthread
RUN cabal build
RUN mkdir -p /out
RUN cp -rf -v dist/build/grendel-exe/grendel-exe /out

# debian:9.4-slim => 22 MB
FROM debian:9.4-slim
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
