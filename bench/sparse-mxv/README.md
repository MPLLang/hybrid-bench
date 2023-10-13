## Inputs
Supports a subset (see below) of .mtx files, although the parser is currently
quite slow, so this takes forever on large inputs:
  $ ./main.mpl.bin @mpl procs 32 -- -input FILE.mtx

To work around this, we have a compressed binary format that loads much faster.
You can make one like this:
  $ ./main.mpl.bin @mpl procs 32 -- -input FILE.mtx -output-bin FILE.mtx.bin

Using the .mtx.bin as input is fast:
  $ ./main.mpl.bin @mpl procs 32 -- -input FILE.mtx.bin


## Supported .mtx files
Only this header is supported at the moment:
```
%%MatrixMarket matrix coordinate real general
```

## Where to get .mtx files
You can get .mtx files from SuiteSparse (https://sparse.tamu.edu/).
Look for the download address for Matrix Market format.

Here's a few that I've found:
  $ wget https://suitesparse-collection-website.herokuapp.com/MM/VLSI/stokes.tar.gz 
  $ wget https://suitesparse-collection-website.herokuapp.com/MM/VLSI/vas_stokes_4M.tar.gz
  $ wget https://suitesparse-collection-website.herokuapp.com/MM/vanHeukelum/cage15.tar.gz