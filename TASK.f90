module Task
use :: mpi
contains

 subroutine GetMaxCoordinates(A, x1, y1, x2, y2)
 implicit none

 real(8), intent(in), dimension(:,:) :: A
 real(8), dimension(size(A(:,1))) :: B
 integer(4), intent(out) :: x1, y1, x2, y2
 integer(4) Aheight, Alength, i, j, k, bottom_border, upper_border, bottom, up
 real(8) previous_Summ, Summ, maxSumm, mpimaxSumm
 integer(4) :: mpiErr, mpiSize, mpiRank, mpimaxRank
 integer(4), dimension(MPI_STATUS_SIZE) :: status


  x1=1
  x2=1
  y1=1
  y2=1
  maxSumm=A(1,1)

  Alength=size(A(:,1))
  Aheight=size(A(1,:))

  call mpi_comm_size(MPI_COMM_WORLD, mpiSize, mpiErr)
  call mpi_comm_rank(MPI_COMM_WORLD, mpiRank, mpiErr)

  do i=mpiRank, Aheight, mpiSize
   B=0
   do j=i,Aheight
    B=B+A(:,j)
    previous_Summ=B(1); bottom=1; up=1
    Summ=previous_Summ; bottom_border=bottom; upper_border=up
    do k=2,Alength
     if (B(k)>(B(k)+previous_Summ)) then
      bottom=k
      up=k
      previous_Summ=B(k)
     else
      up=k
      previous_Summ=B(k)+previous_Summ
     endif
     if (previous_Summ>Summ) then
      Summ=previous_Summ
      bottom_border=bottom
      upper_border=up
     endif
    enddo
    if (Summ>maxSumm) then
     maxSumm=Summ
     y1=bottom_border
     y2=upper_border
     x1=i
     x2=j
    endif
   enddo
  enddo

  if (mpiRank/=0) then
   call mpi_send(maxSumm, 1, MPI_REAL8, 0, 555, MPI_COMM_WORLD, mpiErr)
  else
   mpimaxSumm=maxSumm
   mpimaxRank=0
   do i=1,mpiSize-1
    call mpi_recv(maxSumm, 1, MPI_REAL8, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, status, mpiErr)
    if (maxSumm>mpimaxSumm) then
     mpimaxSumm=maxSumm
     mpimaxRank=status(MPI_SOURCE)
    endif
   enddo
  endif

  call mpi_bcast(mpimaxRank, 1, MPI_INTEGER4, 0, MPI_COMM_WORLD, mpiErr)

  call mpi_bcast(x1, 1, MPI_INTEGER4, mpimaxRank, MPI_COMM_WORLD, mpiErr) 
  call mpi_bcast(x2, 1, MPI_INTEGER4, mpimaxRank, MPI_COMM_WORLD, mpiErr)
  call mpi_bcast(y1, 1, MPI_INTEGER4, mpimaxRank, MPI_COMM_WORLD, mpiErr)
  call mpi_bcast(y2, 1, MPI_INTEGER4, mpimaxRank, MPI_COMM_WORLD, mpiErr)

 end subroutine

end module
