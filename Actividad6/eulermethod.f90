Program eulerp
  Implicit none
  real, parameter :: l = 9.81
  real, parameter :: g = 9.81
  real, parameter :: m = 1
  real :: A, h, wo, t, y, b
  Integer :: j, k
  real, dimension(2) :: resp


wo = sqrt(g/l)
print*, 'Angulo y pasos'
read (*,*) A,h


open(20,file = 'pendata.dat', status = 'unknown')

   
   do k = 0, 7000
   t = float(k) * h
   if (t>6.3) exit
   y = A * cos(wo * t)
   write (20,*) t, y, 1 
   end do


write(20,*) " "
 
 B = A

   
   do k = 0, 7000
   t = float(k) * h
   if (t>6.3) exit
   call eulermethod(A,wo,h,g,l,resp)
   write(20,*) t, resp(1), 2
   A = resp(1)
   wo = resp(2)
   end do

 close(20)
 print*, abs((B-A)/B)
 end program eulerp

!==============================

subroutine eulerm(A,wo,h,g,r,resp)
  implicit none
  real, dimension(2) :: prev
  real, dimension(2) :: nex
  real, intent(in) :: A, wo, h, g, r
  real, dimension(2), intent(out) :: resp
  real :: ap, w, a2, w2
  ap = A
  w =wo
  a2 = h*w
  w2 = -h*g /r*a
  prev = (/a, w /)
  nex = (/a2, w2/)

resp = prev + nex

end subroutine eulerm



