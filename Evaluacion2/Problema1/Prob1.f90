
PROGRAM Triangle
     IMPLICIT NONE
     REAL :: a, b, c, Area, Volumen
     PRINT *, 'Welcome, please enter the&
              &lengths of the 3 sides.'
     READ *, a, b, c
     PRINT *, "Triangles  area:", Area(a,b,c)
     PRINT *, "Parap Volumen:", volumen (a,b,c)
    END PROGRAM Triangle
!==========================================================
    FUNCTION Area(x,y,z)
     IMPLICIT NONE
     REAL :: Area            ! function type
     REAL, INTENT( IN ) :: x, y, z
     REAL :: theta, height
     theta = ACOS((x**2+y**2-z**2)/(2.0*x*y))
     height = x*SIN(theta); Area = 0.5*y*height
    END FUNCTION Area
!===========================================================
    FUNCTION volumen(x,y,z)
     IMPLICIT NONE
      real:: volumen
      real, intent (IN):: x, y, z
      real:: vol
       vol = x*y*z
    END FUNCTION 

 
