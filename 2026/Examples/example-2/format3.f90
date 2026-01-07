
! «This is not your Grandmother's fortran»
! NRIS (Norwegian Research Infrastructure Service), https://www.sigma2.no/nris
! 
! Ole W. Saastad, University of Oslo & NRIS
!
!
! Example of some formatting including writing to an internal
! file (a variable of buffer). 
!
! Including engineering and scientific notation for
! exponents, e.g. 1.55e-30 instrad of 0.155e-29 .
! 
! 
! Written by Ole W. Saastad, UiO
! January 2026.
! 


program format3
  use iso_fortran_env
  implicit none

  character(len=*), parameter :: a='Hello world'
  character(len=80) :: line, form
  integer, parameter :: i=128, j=127
  real, parameter :: x=3.14, y=1.55e-31
  
  print *,a
  write(*,'(3(axxx))') a, a, a
  write(*,'(3(a/))') a, a, a

  print *, i, j
  write(*,'(2i3)') i, j
  write(*,'(2(i3x))') i, j
  write(*,'(sp, 2(i4x))') i, j  
  
  print *, x, y
  write(*, '(2(f6.3))') x, y
  write(*, '(f6.3xe19.3)') x, y
  write(*, '(2(g10.3))') x, y
  ! Engineering format:
  write(*, '(2(en10.2x))') x, y
  ! Scientific format:
  write(*, '(2(es8.2x))') x, y
  
  write(*, '(dc, f6.3xe19.3/)') x, y 

! Write to a buffer named line. 
  write(line, '(dc, f6.3xe19.3)') x, y
  write(*,'(a)') line
  
  form='(f6.3xe19.3)'
  write(*,form)  x, y 
  write(line, form) x, y
  write(*,'(//a)') line


end program format3


