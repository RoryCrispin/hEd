FROM haskell
RUN stack install
ENTRYPOINT ["hEd"]