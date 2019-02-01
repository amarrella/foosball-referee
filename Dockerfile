FROM haskell:8

WORKDIR /opt/server

RUN cabal new-update

COPY ./foosball-referee.cabal /opt/server/foosball-referee.cabal

RUN cabal new-install --only-dependencies -j4

COPY . /opt/server
RUN cabal new-install exe:foosball-referee-exe

CMD ["foosball-referee-exe"]