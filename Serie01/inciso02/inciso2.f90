program inciso2
    implicit none
    real (kind=8) result, verdadero,PI,f,error_rel_verd
    integer i 
    
    PI=2.d0*DASIN(1.d0)
    verdadero = (PI**2)/6d0

    result = 0d0

    open(unit=15,file='normal.txt',status='replace',err=23)
    do i=1,1000,1
        result = result + f(i)
        error_rel_verd = ((verdadero - result)/verdadero)*100
        write(15,*) i,result,error_rel_verd
    end do
    close(unit=15,status='keep',err=23)

    result = 0d0

    open(unit=16,file='inverso.txt',status='replace',err=23)
    do i=1000,1,-1
        result = result + f(i)
        error_rel_verd = ((verdadero - result)/verdadero)*100
        write(16,*) i,result,error_rel_verd
    end do
    close(unit=16,status='keep',err=23)
23 end program inciso2

!******* fuctions **********!

function f(i)
    implicit none
    real (kind=8)f
    integer i
    f = 1d0/(i**2)
end function
