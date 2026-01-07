# Examples part 2

## Introduction

The formatting used in Python has its root from Fortran and C.

A Python statement used to format a float variable can look like this:
```Python
print(format(dd,'02.0f')+'°',format(mm,'06.3f')+"'"," => "\
,format(dd+mm/60.0,'07.4f')+'°')
```
The developers of Python were well acquainted with both Fortran and C. 
Python's formatted output can resemble C or Fortran due to historical 
reasons and the availability of specific formatting tools in Python 
that mimic those styles.

Hence learning Fortran formatting is beneficial for today's Python 
programmers. 

## Formatting 

The simple statement print is mostly used for simple and easy 
printout to the terminal. While write is a more elaborate function.

The examples format 1 through 3 explore some different usages. 

The possibility to change the decimal comma from dot to comma can come
in handy in Norwegian and some other European languages where comma is
used a the decimal comma.

For numbers with exponents the engineering notation might be 
more familiar for many sciences. Usage of 'EN' or 'ES' instead of 'E'
is a nice option.

Difference in EN and ES: 

The ES descriptor produces output in standard scientific notation,
where the significand (the part before the 'E') is greater than or
equal to 1 and less than 10, except when the value is zero. This
format is generally considered the most human-readable form of
scientific notation.

The EN descriptor produces output in engineering notation, where the
exponent is constrained to be a multiple of three (e.g., E+03, E+06,
E+09, E-03, etc.), and the non-zero significand is in the range of 1
to 1000. This in line with every day prefix p,n,µ,m,k,M,G etc used in 
the metric system. 

## Units

### History

In the early days of computing a unit was things like a tape drive, a
card puncher/reader. Each of these had a fixed number and you directed
the I/O to these units. The concept of files do not make sense on these
devices. When disks with file systems were introduced the concept remained
and unit became a file handle for a disk file. 

### Standard units

In most cases the standard input and output is used 
and the '*' character is used in the write statement to
interact with the terminal. 
However, in some cases more fine grain control is needed. 
Linux make use of 3 units for the terminal, stdin, stdout and stderr.
These are available in Modern Fortran:

- INPUT_UNIT
- OUTPUT_UNIT
- ERROR_UNIT

These correspond to Linux stdin, stdout and stderr. 
Do not bother to remember the units numbers, use the predefined
constants. 

Try running the units.f90 program and enter a letter and see how
the output is going to stdout and stderr 

```terminal
[olews@cumen example-2]$ ./a.out 
Give en integer >n
 Error

./a.out 2> /dev/null 
Give en integer >m
[olews@cumen example-2]$
```
Notice how the error message is redirected to the device call null,
which is essential a dump device. 


## File I/O 

### Writing and reading from and to the storage device

A file need to be opened and closed. It can be opened for
reading, writing or both read and write. 

Please not the newer syntax regarding the unit numbers. 
Before one needed to select a unit number not used before and 
needed to remember this units in order not to use the same unit number
for a different file. With today's standard the open statement will
assign a number that is not used. 

```Fortran
open(newunit=fileunit, file=filename, action='write')
```
The variable fileunit is now automatically assigned for us to a 
safe and usable unit number. 

### Formatted and unformatted

File I/O can be either formatted (normally ascii text) and unformatted 
binary bytes. 

The unformatted I/O is used to store anything in raw binary formats as
byte by byte. A vector of real 32 bits numbers will use only 4 bytes per
number and no conversion is done during  the I/O process. The most efficient 
way of writing and reading data.

The only catch is that the binary raw files might not be portable between
different computer architectures. Some care need to be taken when moving a 
binary file from one architecture to another. 


