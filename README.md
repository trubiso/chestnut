# Chestnut

This is the first C++ compiler for my programming language, Chestnut.

It is under heavy development, so expect segmentation faults and bugs
everywhere :-)

## Building
You must have the LLVM and Boost libraries. On Arch Linux, you may install them
by running `sudo pacman -S llvm boost`.

To actually build, use `make build`, `make run` (to compile and run the file
`my_module`) or `make test` (to run all compiler-internal tests).
