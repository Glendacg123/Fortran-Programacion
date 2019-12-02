subroutine Senox(x,f,nt)

implicit none
 real(kind=8):: x, f, m1, g, factorial
 integer:: i, nt, m
  
 f=0

 do i=0,nt-1
 m = (-1)**i
 g= 2*i+1 
 m1= x**g/factorial(g)
 f= f+(m*m1)
! print*, i, x, f, m, m1
 end do
end subroutine Senox
