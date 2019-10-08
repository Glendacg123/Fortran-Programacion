Program Problema1
 Implicit none 


!definimos valores

Integer :: i
real :: x, y, sen1, sen2, E, SinX
real, parameter :: pi = 3.1416, dt = 0.01


!Problema1
OPEN(unit = 6, file = "seno.dat", access = "Append")
  Do i = 0, 10000
  x = (-1.0*pi)+(dt*i)
  
 sen1 = ((12671.0/4363920.0)*(x**5.0))-((2363.0/18183.0)*(x**3.0))+x
 sen2 = 1.0+((445.0/12122.0)*(x**2.0))+((601.0/872784.0)*(x**4.0))+((121.0/16662240.0)*(x**6.0))
 
  y=sen1/sen2

IF(x>=pi) EXIT

!Resultado
 Print*, x, y
 Write (6,*) x, y

END DO

close(6)

 OPEN (2,FILE='ErrorSinP.dat')
  DO i=0,31415926,1000
    x=i*0.0000001
    
    SinX = Sin(x)
  
      E=(SinX-y)/(SinX)
       WRITE(2,*) x,E
  END DO
 close(2)

end program problema1 



