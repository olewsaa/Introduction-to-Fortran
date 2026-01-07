# Examples part 1

## Hello world

The simplest possible hello world in fortran is the
hello-minimalistic.f90 (should have been named h.f).

A more elaborate hello world with different ways of emitting text
using both print (print to terminal, stdout) and write which write
formatted and unformatted data to terminal, buffers or files.

In addition there are two examples of how to write hello world using
assembly language (x86-64) machine code. It's added in to show how
instructions executed by the processor look like. Possibly the only
time you'll ever encounter machine instructions.  It can however be
beneficial to look hos the Fortran compiler translated certain blocks
or loops and how the vector instructions are used. The compiler do not
always generate the optimal code.

The code itself contain comments on how to compile, but here's
a couple of examples.

- gfortran hello.f90  ; Generate executable a.out
- gfortran gfortran hello.f90  -o hello ;  Generate executable hello

Compiler options is by itself a separate workshop or semester. 

## Goto

An example how really bad code can look like. Don't be surprised 
if you come across or get it such code handed over to you with 
request to rewrite or clean up. 


## Variables

This example is written to show how to work with variables. The human
way where you think of which precision is needed. Or the machine way
with the size and number of bits in the variable.


## Derived types

The program location illustrate the usage of derived types.
The derived data type location hold a latitude and a longitude. 

## Spacecraft Mariner

One missing comma might have disastrous consequences. 
Remember that in old Fortran spaces were not significant hence
a variable called DO 15 I is a valid variable, with the name 'DO15I'.


## Overflow - what is 127 + 1 ?

A classic overflow example. Trivial for school children, but 
not so for computers. Keep control of your variables and range. 


## If then else

Flow control using if then else construct. 

## Flow Control 

Flow control and type of decisions controlling the flow of program.
while and loops.
















