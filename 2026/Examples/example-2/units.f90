
! «This is not your Grandmother's fortran»
! NRIS (Norwegian Research Infrastructure Service), https://www.sigma2.no/nris
! 
! Ole W. Saastad, University of Oslo & NRIS
!
!
! Example of units
! file (a variable of buffer). 
! 
! Written by Ole W. Saastad, UiO
! January 2026.
! 


program units
  use iso_fortran_env
  implicit none
  integer :: iostatus
  integer :: n
  
  write(OUTPUT_UNIT,'(a)', advance='no') 'Give en integer >' 
  
  read(INPUT_UNIT,'(i2)', iostat=iostatus) n
  if (iostatus==0) then  
     write(OUTPUT_UNIT,*) 'Hello World'
  else
     write(ERROR_UNIT,*) 'Error, no integer given'
  end if
end program units


