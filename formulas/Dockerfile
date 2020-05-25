# Adapted from https://hub.docker.com/_/haskell

FROM haskell:8

RUN apt-get update && apt-get install -y libblas3 libblas-dev liblapack-dev liblapack3

WORKDIR /opt/formulas

# Add the stack.yaml to set the stack resolver.
COPY stack.yaml /opt/formulas/stack.yaml

# Add just the .cabal file to capture dependencies
COPY formulas.cabal /opt/formulas/formulas.cabal

Run stack setup

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN stack install --only-dependencies -j4 

COPY . /opt/formulas/

RUN stack build 

ENTRYPOINT ["stack", "run", "--", "haskfile"]
