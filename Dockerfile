FROM haskell:8

WORKDIR /opt/server

RUN cabal update

COPY ./foosball-referee.cabal /opt/server/foosball-referee.cabal

RUN cabal install --only-dependencies -j4

COPY . /opt/server
RUN cabal install

CMD ["foosball-referee-exe"]