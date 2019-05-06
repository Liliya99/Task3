module Task
!use omp_lib
contains
subroutine GetMaxCoordinates(A, x1, y1, x2, y2)
implicit none

real(8), intent(in), dimension(:,:) :: A
real(8), dimension(size(A(:,1))) :: B
integer(4), intent(out) :: x1, y1, x2, y2
integer(4) Aheight, Alength, i, j, k, bottom_border, upper_border, bottom, up
real(8) previous_Summ, Summ, maxSumm

x1=1
x2=1
y1=1
y2=1
maxSumm=A(1,1)

Alength=size(A(:,1))
Aheight=size(A(1,:))


do i=1,Aheight
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
   x1=bottom_border
   x2=upper_border
   y1=i
   y2=j
  endif
 enddo
enddo

end subroutine
end module
