program noaire
  implicit none

  ! definimos las variables
  real, parameter :: pi = 3.1415927
  real, parameter :: g = 9.81
  real :: t, x, y, dt, m, Vx0, Vy0
  real :: Vt, Vx, Vy, Vi, a, Xmax, Ymax, ty
  integer :: np, n

  x = 0
  y = 0
  t = 0
  dt = 0.1
  Vi = 44.7
  Vt = -33
  np = 200
  m = 0.145
  a = 45
  ty = 0

  open(unit = 11, file = "pparabola.dat", status = "unknown")
   
  a = a * pi / 180.0
  
  Vx = Vi * cos(a)
  Vy = Vi * sin(a)
  
  do n = 0, np
  
  x = Vx * t
  y = (Vy*t) + (0.5 * (-g) * (t**2))
  t = t + dt

 print*, x, y
 
 if (y <0.0) exit

 Write (11,*) x, y

 write (11,*)" "
 write (11,*)"# "
 write (11,*)" "
 
 end do
  
 Xmax = ((Vi**2) * sin(2*a))/g
 
 print*, "El alcance maximo en x es de:" , Xmax , "En un tiempo de: " , t

 Ymax = ((Vi ** 2) * (sin(a)*sin(a))) / (2*g)
  
 ty = t/2
 
 Print*, "La altura maxima es de:", Ymax , "En un tiempo de:" , ty

 close (11)
 
 end program noaire
