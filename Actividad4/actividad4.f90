program Resistencia
  implicit none

  ! definimos las variables
  real, parameter :: pi = 3.1415927
  real, parameter :: g = 9.81
  real :: t, Sx, Sy, dt, m, Vx0, Vy0
  real :: Vt, Vx, Vy, Vi, k, a, d, Ymax
  integer :: np, n

  Sx = 0
  Sy = 0
  t = 0
  dt = 0
  Vi = 44.7
  Vt = -33
  np = 55
  m = 0.145
  k = 0.0431
  a = 45

  open(unit = 11, file = "Coordenadas.dat", status = "unknown")
   
  a = a * pi / 180.0
  
  Vx0 = Vi * cos(a)
  Vy0 = Vi * sin(a)
  
  do n = 0, np
 
    
  Sx = ((m/k) * Vx0) * (1 - exp( (-k/m) * t))
  Sy = (((-m*g)/k)*t) + (m/k)*(Vy0 + ((m*g)/k)) * (1 - exp( (-k/m) * t ))
  Vx = Vx0 * (exp((-k/m) * t))
  Vy = ((-m*g)/k) + (Vy0 + ((m*g)/k)) * exp (((-k)/m)*t)
  t = t + dt
  
  if (Vy > Vt) then
      Vy = Vt
  end if 

  if (Vx > Vt) then
      Vx = Vt
  end if

 print* ,"X: ", Sx, "y: ", Sy
 Write (11,*) Sx, Sy

 write (11,*)" "
 write (11,*)"# "
 write (11,*)" "
 
 end do


 Ymax = ((Vi ** 2) * (sin(a)*sin(a))) / (2*g)

 Print*, "La altura maxima en y es de :", Ymax
 d = ((Vi**2) / g) * sin(2*a)

 Print*, "El desplazamiento maximo en x es de :", d
 
 close(11)
 
 end program Resistencia
  
