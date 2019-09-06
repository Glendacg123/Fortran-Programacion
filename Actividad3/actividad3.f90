program projectile
  implicit none

  ! definimos constantes
  real, parameter :: g = 9.8
  real, parameter :: pi = 3.1415927
  real, parameter :: dt=0.1


  ! definimos las variables
  real :: a, t, u, x, y, dtang
  integer :: n
  
  ! Varibles integer
  integer :: i,j,m

  ! Leer valores para el ángulo a, el tiempo t, y la velocidad inicial u desde la terminal
  WRITE(*,*) 'Dame la rapidez inicial'
  READ(*,*) u

  ! convirtiendo ángulo a radianes
  dtang=15.0 * pi / 180.0


  ! Numero de puntos Grafica

  OPEN(unit=11 ,FILE="SALIDAS.DAT" ,status="unknown")
  Do j = 1,6
  a=float(j)*dtang
  Do i=0 ,200
   t=(i)*dt
  
  
  !las ecuaciones de la posición en x y y
  x = u * cos(a) * t
  y = u * sin(a) * t - 0.5 * g * t * t
 
	IF(y<0.0)EXIT

  WRITE(11,*)x,y
  End do
  WRITE(11,*) " "
  End do
  close (11)
 

  end program projectile
 
