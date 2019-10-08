PROGRAM problema2E
IMPLICIT NONE

Real(kind=8),external::Expx,Expr,Expu
Real(kind=8)::ExpX,x,y,r,u,e
Integer :: i

Open (7, File = "error02.dat")
DO i = -31415926,31415926,1000
   x = i*0.0000001
   
   ExPx = Exp(x)
   y = Expx(x)
   e = ExpX-(y/ExpX)
Write(7,*) x,e
End DO
close(7)

Open (8, File = "error11.dat")
DO i = -31415926,31415926,1000
   r = i*0.0000001
   ExpX = Exp(r)
   y = Expr(r)
   e = ExpX-(y/ExpX)
Write(8,*) r,e
End DO
close(8)

OPen (9, File = "error20.dat")
DO i = -31415926,31415926,1000
   u = i*0.0000001
   ExpX = Exp(u)
   y = Expu(u)
   e = ExpX-(y/ExpX)
Write(9,*) u,e
End DO
close(9)
End PROGRAM problema2E

!=============
FUNCTION Expx(x)
!=============
IMPLICIT NONE

REAL(kind=8),intent(in)::x
REAL(kind=8)::Expx,a,b

a=1.0

b=1-x+(x**2)*(1.0/2.0)

Expx=a/b

END FUNCTION Expx
!=============
FUNCTION Expr(r)
!=============
REAL(kind=8),intent(in)::r
REAL(kind=8)::Expr,a,b

a=1+r*(1.0/2.0)

b=1-r*(1.0/2.0)

Expr=a/b

END FUNCTION Expr
!=============
FUNCTION Expu(u)
!===================
IMPLICIT NONE

REAL(kind=8),intent(in)::u
REAL(kind=8)::Expu,a,b

a=1+u+(u**2)*(1.0/2.0)

b=1.0

Expu=a/b

END FUNCTION Expu





