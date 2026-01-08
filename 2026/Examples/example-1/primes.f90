!
! «This is not your Grandmother's fortran»
! NRIS (Norwegian Research Infrastructure Service), https://www.sigma2.no/nris
! 
! Ole W. Saastad, University of Oslo  & NRIS
!
! Prime number, code taken from the internet. 
! 
!  
!  
!
PROGRAM Primes
  IMPLICIT NONE  
  INTEGER :: Range, CurrentNumber, Divisor
  LOGICAL :: is_prime

  ! Set the upper limit for finding primes
  Range = 100
  PRINT *, "Prime numbers up to ", Range, ":"

  ! 2 is the first prime number, handle it separately
  PRINT *, 2
  
  ! Loop through all odd numbers from 3 up to the defined range
  DO CurrentNumber = 3, Range, 2
     is_prime = .TRUE.
     ! Check for divisibility by odd numbers starting from 3
     Divisor = 3
     DO WHILE (Divisor*Divisor <= CurrentNumber)
        IF (MOD(CurrentNumber, Divisor) == 0) THEN
           is_prime = .FALSE.
           EXIT ! Exit the inner loop if a divisor is found
        END IF
        Divisor = Divisor + 2
     END DO
     
     ! If the number is still marked as prime after all checks, print it
     IF (is_prime) THEN
        PRINT *, CurrentNumber
     END IF
  END DO
  
END PROGRAM Primes
