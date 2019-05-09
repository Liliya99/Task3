Program main
use :: mpi
use :: Task
implicit none
real(8), dimension(:,:), allocatable :: A
integer(4) :: x1, y1, x2, y2, i, height, length
integer(4) :: mpiErr, mpiSize, mpiRank

call mpi_init(mpiErr)
write(*,*)"proverka"
call mpi_comm_size(MPI_COMM_WORLD, mpiSize, mpiErr)
call mpi_comm_rank(MPI_COMM_WORLD, mpiRank, mpiErr)

open(1,file='data.dat')
read(1,*) height, length
allocate(A(height,length))
do i=1,height
 read(1,*)A(i,:)
enddo
close(1)
call GetMaxCoordinates(A, x1, y1, x2, y2)

call mpi_finalize(mpiErr)

write(*,*)x1, y1, x2, y2
deallocate(A)
end
