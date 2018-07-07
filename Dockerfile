FROM nixos/nix:2.0.4

RUN apk update && apk add cabal
RUN mkdir -p /app/src
WORKDIR /app/src
COPY . /app/src/
RUN nix-shell --command 'cabal update'
RUN nix-shell --command 'cabal configure && cabal build'
CMD nix-shell --command 'dist/build/grendel-exe/grendel-exe'
