FROM haskell:9.6.4

WORKDIR /opt/star-trek-action-figures-inc

RUN cabal update

# Add just the .cabal file to capture dependencies
COPY ./star-trek-action-figures-inc.cabal /opt/star-trek-action-figures-inc/star-trek-action-figures-inc.cabal

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN cabal build --only-dependencies -j4

# Add and Install Application Code
COPY . /opt/star-trek-action-figures-inc
RUN cabal install

CMD ["star-trek-action-figures-inc"]