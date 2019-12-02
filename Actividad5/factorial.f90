
function factorial(g) 

 implicit none 

  integer::i,factorial
  real(kind=8):: g

  do i=1,int(g) 

   factorial=factorial*i 

  end  do 

 end function factorial
