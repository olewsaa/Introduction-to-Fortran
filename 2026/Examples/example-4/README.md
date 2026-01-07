# Examples part 4

## Introduction

Functions and subroutines are important to breaking up a program into 
manageable pieces. 

There is sharp distinction between functions and subroutines. 

Functions take an input and return a value. 

subroutines take input and output via parameters. 

Input parameters in functions are not supposed to be updates (there
are mechanisms to enforce this). Input parameters are only for input. 
The output is returned by the function.

Subroutines do not have a return value, but the parameters can be
input only, output only or both read and write.


Fortran contain syntax to enforce this using the keyword intent. 


## Fortran variables are by reference

All Fortran variables are references to the memory. Any updates on any
variable is reflected in memory and all references to it. As seen in 
function2.f90 this can have unexpected side effects and consequences. 

There are ways to prevent side effects from happen. function3.f90
explore this.

## pure functions and subroutines

Pure functions and subroutines have no side effects.
A safe way to make sure nothing outside the scope of 
routines are affected.

## Fixed allocations, recursive functions

A function that declare variables within only set aside one copy of the
variables. Hence calling the same function from itself might lead to 
errors. To make  function recursive (calling itself) the keyword recursive
must be prefixed the function declaration. 


## Elemental functions

Operates element by element of the input. As the operations are 
elemental it can take variables on any rank. Making it possible 
to write very compact and simple code.



