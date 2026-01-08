!
! «This is not your Grandmother's fortran»
! NRIS (Norwegian Research Infrastructure Service), https://www.sigma2.no/nris
! 
! Ole W. Saastad, University of Oslo  & NRIS
!
! Manipulation of vectors, shift, implied loops. 
! 
! 
! Written by Ole W. Saastad, UiO
! January 2026.
! 
!


! Generate a matrix :
!
!   1   2   3   4   5   6   7   8   9  10
!   2   3   4   5   6   7   8   9  10   1
!   3   4   5   6   7   8   9  10   1   2
!   4   5   6   7   8   9  10   1   2   3
!   5   6   7   8   9  10   1   2   3   4
!   6   7   8   9  10   1   2   3   4   5
!   7   8   9  10   1   2   3   4   5   6
!   8   9  10   1   2   3   4   5   6   7
!   9  10   1   2   3   4   5   6   7   8
!  10   1   2   3   4   5   6   7   8   9
!
!
! A = reshape([ ( (mod(i+j-2,10)+1 , i=1,10), j=1,10) ], [10,10] ) 
!


program mat
  use iso_fortran_env
  implicit none
  integer(int16) :: i,j
  integer(int16), dimension(10,10) :: A, B
 

  A = reshape([ ((i, i=1,10), j=1,10) ], [10,10])

  do i=1,10
     A(:,i) = cshift(A(:,i),i-1)
     print *, A(:,i)
  end do
  print *

  B = A
  A = matmul(A,B)

  do j=1,10
     print *,A(:,j)
  end do

  print *,'Simpler code'
  
  A = reshape([ ( (i+j-1, i=1,10), j=1,10) ], [10,10])
  where (A>10) A=A-10

  print '(10i4)', A
  print *,'A*B :'
  B = A
  A = matmul(A,B)
  print '(10i4)', A


  print *,'Simple'
  A = reshape([ ( (mod(i+j-2,10)+1 , i=1,10), j=1,10) ], [10,10] )
  print '(10i4)', A
  B = A
  A = matmul(A,B)
  print '(10i4)', A
  
end program mat  
