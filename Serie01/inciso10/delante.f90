program inciso10_haciadelante
    implicit none
    real (kind=8) a,h,f,dfdx
    integer i
    open(unit=18,file='derivada_adelante.txt',status='replace',err=23)
    !Valor donde se aaluara la derivada
    a = 1.d0
    ! 2*h (2 veces el tamaño del paso)
    h = 2.d0
    write(18,*) '         dfdx'
    do i=1,60,1
        h = h*(0.5d0)
        !Derivada númerica de f'(a)
        dfdx = (f(a+h)-f(a))/h ! Error por resta 
        write(18,*) dfdx
    end do 
    close(unit=18,status='keep',err=23)
23 end program inciso10_haciadelante

!*********** functions ***********!
function f(x)
    implicit none
    real (kind=8) f,x
    f = -5.d0*x**3 + 20.d0*x + EXP(x**2) 
end function

