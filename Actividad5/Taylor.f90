Program Taylor

implicit none
 
real(kind=8):: x, y, h, a, b
integer::j, n, nt

a=0 
b=1
n=100
h=(b-a)/n
nt=100

  OPEN(1,FILE="Senox1",STATUS="UNKNOWN")

 do j=0,n
   x=a+j*h
 call Senox(x, y, nt)
 print*,  x, y
     WRITE(1,*)x,y
     WRITE(1,*)
     WRITE(1,*)
     WRITE(1,*)
end do
 close(1)

end program Taylor
