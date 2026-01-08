!
!
!
! «Introduction to Fortran» training. 
! NRIS (Norwegian Research Infrastructure Service), https://www.sigma2.no/nris
! 
! Ole W. Saastad, University of Oslo  & NRIS
! 
! Overflow in MPI this is a C program. Illustrating that overflow is
! an actual problem when using MPI. 
!
! Ole W. Saastad, UiO.
!
!
!
!  Load a module that provode  MPI GCC
!  Saga: module load OpenMPI/5.0.7-GCC-14.2.0
!
!

/* C MPI program start here 

   Compile with mpicc.
   mpicc mpi-longint.c -o mpi-longint
   run
   mpirun -np 2 ./mpi-longint

   Notice what happen when N becomes to large (larger than 2^31).
*/ 

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "mpi.h"


/* https://software.intel.com/en-us/forums/intel-clusters-and-hpc-technology/topic/361060 */

  */ Try with N of different sizes */
  
#define N    3000000000 
/* #define N    2000000000 */
/* #define N 1800000000  */
  
int main(int argc, char* argv[]) {
  int rank,master,size;
  long  i,l;
  double *p;
  MPI_Status  st;
  MPI_Aint k;

  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  
  master=(rank==0);

/* Here lies the problem, these are longint (64 bits),
   but the variable receining the parameter is only
   32 bit and you have an overflow
*/
  l=N;
  k=N;
  printf("Myrank is %d of %d\n",rank,size);
  if (master) printf("size MPI_Aint %d\n",sizeof(k));
  if (master) printf("size l=%d\n",sizeof(l));

  if (master) printf("l= %ld  log2(l)=%lf\n",l,log2(l)); 

  p = malloc(sizeof(double)*l);
  
  if (master) printf("Size of p %d MiB\n",(int)((sizeof(double)*l)/(1024*1024)));
  if (master) for (i=0;i < l; i++) p[i]=(double)1.0*i;
  
  if(master) MPI_Send(p, k, MPI_DOUBLE, 1, 1, MPI_COMM_WORLD);
  else MPI_Recv(p, k, MPI_DOUBLE, 0, 1, MPI_COMM_WORLD, &st);          
  
  printf("p[N-1] rank %d %lf\n",rank,p[N-1]); 
  free(p);
    
  MPI_Finalize();
}

