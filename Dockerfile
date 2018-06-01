FROM haskell:8.0.2
RUN mkdir -p /app/src
WORKDIR /app/src
COPY . /app/src
RUN stack build && stack install && rm -rf .stack/.stack-work
CMD grendel-exe
