#!/bin/sh
cd libffi-src
cd build
../configure --disable-shared --enable-static
make
