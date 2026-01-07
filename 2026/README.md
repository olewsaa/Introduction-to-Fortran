# Introduction to Fortran 2026 edition

## Background for training

A scientific training. Fortran is mostly used where calculations
are required. 

A working knowledge of programming. Python knowledge and 
experience is fine. 

As many students will be exposed to old legacy Fortran codes 
this training will provide some background and introduction
to the latest developments. Fortran has changed dramatically from
the 1977 standard via major revision to a modern language in 1991
with object orientation introduced in 2003. With 2008, 2018 and
recently 2023 updates. 

## Modern Fortran


Modern Fortran focus on higher level operations, like vector
syntax where large data structures are operated upon as they were 
scalars, users can write elemental functions that operate on multi 
dimensional vector as they were scalars. 
It also operate on objects like any other modern language. 

Being strongly typed is also a key feature. Keeping control of
range and precision is important. The language do not make any assumptions 
about the under laying hardware. User can just ask for types that fit the
current need, precision and range. The compiler will then try to map
these requests to the under laying machine hardware. 

## Fortran and Python

Many numerical libraries used in Python are written in Fortran
(some also in C). The much higher performance gained using Fortran 
is a major driver. A speedup of 100x is not uncommon. Hence learning 
Fortran is beneficial for many Python programmers. Calling Fortran functions 
and subroutines from Python is relatively easy. 

## Prerequisites

Install a recent Fortran compiler on your laptop / PC.
Gfortran 14 or 15 are preferred versions.

- Ubuntu: sudo apt install gfortran
- Mac: brew install gcc (This install gfortran also)
- Windows: see https://www.msys2.org/ (an example)

## Fortran Examples 

All the examples have been tested on Gfortran and should work 
with no issues. Having said that, there might be issues with 
older Gfortran compilers or compilers from other sources like
flang, Intel or NVHPC. 

Some features were introduces in the 2003 standard, some introduces in the 2008
standard while yet some in the 2018 standard. With the new 2023 standard about
to be implemented yet more features will be available. 

Hence not all examples might run with older versions. 


