# Backend

src/Lib.hs : Main definitions of server.

src/Backend.hs : Library using "persistent" and "servant".

src/Generator.hs : Library using "servant-elm".

# How to start server

$ stack exec backend

# How to generate Api.elm

$ stack test
