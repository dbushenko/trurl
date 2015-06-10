Import database from dump.sql

If authentication used -- the dump contains one user 'root' with password 'root' and role 'admin'.

Run it:

    cabal sandbox init
    cabal install --dependencies-only
    cabal build
    cabal run

Navigate browser to http://localhost:3000/articles