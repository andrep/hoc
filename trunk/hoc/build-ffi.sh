#!/bin/sh
cd libffi-src
mkdir -p build
cd build
../configure --disable-shared --enable-static
make
