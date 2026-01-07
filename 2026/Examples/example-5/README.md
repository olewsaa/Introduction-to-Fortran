# Examples part 5

## Introduction

Dynamic memory allocation is important to manage memory in an effective way. 
Reuse of memory during the run of a program code. 

## Pointers

A pointer is a types variable pointing to a place in memory there the variable 
start. After the pointer has been set to point to the variable any  operation
on the pointer is just like operating on the variable itself. 

```Fortran
real(real64), allocatable, target, dimension (:) :: x
real(real64), pointer, dimension(:) :: pt
```

The target, the variable declared with the attribute target (meaning
that a pointer can be set to point to it) can be a target for the 
pointer, which itself is a typed and rank specified variable. 

The pointer can now be used to point to different vectors, but they 
all must be the same type and rank size. Fortran also support polymorphic
pointers.

### source and mold option

The mold option can be used with allocate to fill the allocated vector
with values. 

Mold is just what the word suggest, it's just a mold, but it's not
filled with anything. Just the allocated space is identical top the 
mold option argument. It makes an identical vector with no values set.
```Fortran
allocate(z, mold=pt)
```

Source on the other hand fill the newly allocated space with values 
from source.
```Fortran
allocate(z, source=pt)
```
Making the two identical.
Source can be a constant, like
```Fortran
allocate(z(vectorsize), source=3.14_real64)
```
alternatively way to fill the vector, using vector syntax.
```Fortran
z=3.14_real64
z=pt
```
Some tests suggest that this is faster than using the source option to allocate.

The example alloc4.f90 exposes what happen when data in memory is not 
cleared after deallocating. Data is still present in memory. Allocating
one more time might reuse the newly deallocated space and it appear to be
data in the vector that should have no data at all since mold do not copy
data from the mold vector. 

Try to comment out the z=0.0 in line 76 in alloc4.f90. Try also to 
remove the comments in line 88 and 92, but run with a small size like 10.
You might observe some interesting effects of old data suddenly being 
present. 


### Polymorph pointer

Polymorph pointer is a bit more high level. 

The example alloc5.f90 show how to make a polymorph pointer that can point 
to variables of different types. 

Notice that handling of the vectors still require to be handled by
their data types. Ref. the printout section.


## Preprocessor

The C-preprocessor is a macro processor that can perform text
operation on the source code before being fed into the compiler.
Any source code ending with a capital F will be subjected to the 
preprocessor. 

```Fortran
#ifndef N
#   define N    20000000
#endif
```

```shell
gfortran -DN=200000000 alloc7.F90
```

The N is then set at compile time. Very practical that no edit
of the source code is needed to change fixed constants in the 
source code, it's done at compile time.  


## Static versus dynamic allocation

The example alloc8.F90 is a demonstration to show performance difference 
between dynamic and static memory allocation. 

How much this is relevant for large scale scientific applications are
debatable. 





