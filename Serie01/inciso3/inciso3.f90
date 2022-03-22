program inciso3
    implicit none
    real f,epsilon_t,epsilon_a,coseno,valor_t,valor_anterior,PI
    integer i
    
    open(unit=17,file='serie_coseno.txt',status='replace',err=23)
    
    PI=2*DASIN(1d0)
    coseno = 1
    valor_t = cos(PI/4)
    valor_anterior = 1

    do i=1,5,1
        coseno = coseno+f(i)

        epsilon_t = 100*(valor_t-coseno)/valor_t
        epsilon_a = ABS(100*(coseno-valor_anterior)/coseno)
        
        valor_anterior = coseno
        write(17,*) i,coseno,epsilon_t,epsilon_a
    end do
    close(unit=17,status='keep',err=23)
23 end program inciso3

!*********** functions ***********!
function f(k)
    implicit none
    real PI,f,factorial
    integer k

    PI=2*DASIN(1d0)
    f = (((-1)**k)*((PI/4)**(2*k)))/factorial(2*k)
end function

function factorial(n)
    implicit none
    real factorial
    integer i,n

    factorial=1

    do i=1,n,1
        factorial = factorial*i
    end do
end function
