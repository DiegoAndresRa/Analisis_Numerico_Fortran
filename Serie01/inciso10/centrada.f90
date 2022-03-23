program inciso10_centrada
    implicit none
    real (kind=8) a,h,f,dfdx
    integer i
    open(unit=19,file='derivada_centrada.txt',status='replace',err=23) 
    
    a = 1.d0
    h = 2.d0
    
    write(19,*) '         dfdx'
    do i=1,60,1
        h = h*(0.5d0)
        dfdx = (f(a+h)-f(a-h))/(2.d0*h) ! Error por resta 
        write(19,*) dfdx
    end do 
    close(unit=19,status='keep',err=23)
23 end program inciso10_centrada

! **** FUNCIÓN A DERIVDAR ************
function f(x)
    implicit none
    real (kind=8) f,x
    f = -5.d0*x**3 + 20.d0*x + EXP(x**2) 
end function

