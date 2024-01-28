FROM fpco/stack-build:lts-22.5 as build
RUN mkdir /opt/build
COPY . /opt/build
RUN cd /opt/build && stack --no-terminal build \
    --system-ghc \
    --ghc-options '-j4 +RTS -A64m -n2m -RTS' \
    --copy-bins \
    --local-bin-path \
    ./

FROM ubuntu:22.04
RUN mkdir -p /opt/hs-bomb-party
WORKDIR /opt/hs-bomb-party
RUN apt-get update && apt-get install -y ca-certificates libgmp10
COPY --from=build /opt/build/hs-bomb-party .

ENV APP_PORT 80
EXPOSE 80

CMD ["/opt/hs-bomb-party/hs-bomb-party", "+RTS", "-N"]