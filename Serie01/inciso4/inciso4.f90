program inciso4
    implicit none
    real f,epsilon_t,epsilon_a,seno,valor_t,valor_anterior,PI
    integer i
    
    open(unit=17,file='serie_seno.txt',status='replace',err=23)
    
    PI=2*DASIN(1d0)
    valor_t = sin(PI/4)
    valor_anterior = 0

    do i=0,5,1
        seno = seno+f(i)

        epsilon_t = 100*(valor_t-seno)/valor_t
        epsilon_a = ABS(100*(seno-valor_anterior)/seno)
        
        valor_anterior = seno
        write(17,*) i,seno,epsilon_t,epsilon_a
    end do
    close(unit=17,status='keep',err=23)
23 end program inciso4

!*********** functions ***********!
function f(k)
    implicit none
    real PI,f,factorial
    integer k

    PI=2*DASIN(1d0)
    f = (((-1)**k)*((PI/4)**((2*k)+1)))/factorial((2*k)+1)
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
