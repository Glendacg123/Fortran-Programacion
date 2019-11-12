program eulerp
    implicit none 
    real, parameter :: r = 9.81     
    real, parameter :: g = 9.81
    real, parameter :: m = 1          
    real :: A, h, wo, t, y, B 
    integer :: j, k
    real, dimension(2) :: resp

wo = sqrt(g / r)           
print*, 'Ángulo y tamaño de pasos'
read(*,*) A, h

open (5, file = 'pendulodata.dat', status = 'unknown') 

       do k = 0, 7000
       t = float(k) * h 
       if (t>6.3) exit 
       y = A * cos(wo * t)
       write(5,*) t, y, 1
       end do

write(5,*) " "
B = A

       do k = 0, 7000
       t = float(k) * h
       if (t>6.3) exit 
       call eulermethod(A,wo,h,g,r,resp)    
       write(5,*) t, resp(1), 2
       A = resp(1)
       wo = resp(2)
       end do
close (5)

print*, abs((B-A)/B)    
end program eulerp

!=====================
subroutine eulermethod(A,wo,h,g,r,resp)
    implicit none
    real, dimension(2) :: prev
    real, dimension(2) :: nex
    real, intent(in) :: A, wo, h, g, r
    real, dimension(2), intent(out) :: resp
    real :: ap, w, a2, w2
    ap = A
    w = wo
    a2 = h * w
    w2 = -h * g / r * a
    prev = (/a, w /)
    nex = (/a2, w2/) 

resp = prev + nex

end subroutine eulermethod



