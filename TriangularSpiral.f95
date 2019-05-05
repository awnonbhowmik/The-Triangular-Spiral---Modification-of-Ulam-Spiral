program triangular_spiral
    implicit none
    integer::i,j,n,x,y,dimn
    integer,allocatable::a(:)
    character(1),allocatable::spiral(:,:)
    character(2)::sstr
    character(10)::fmt

    write(*,*)"Enter the dimension (odd number):"
    read(*,*)dimn
    if(mod(dimn,2)==0)then
        write(*,*)"Odd number required as input"
        stop
    end if

    allocate(a(dimn*dimn))
    allocate(spiral(dimn,dimn))
    do i=1,dimn*dimn
        a(i)=i
    end do
    spiral(dimn,dimn)=" "

    n=1
    x=dimn/2+1
    y=x
    if(istriangular(a(n))) spiral(x,y)="*"
    n=n+1

    do i=1,dimn-1,2
        do j=1,i
            x=x+1
            if(istriangular(a(n))) spiral(x,y)="*"
            n=n+1
        end do

        do j=1,i
            y=y-1
            if(istriangular(a(n))) spiral(x,y)="*"
            n=n+1
        end do

        do j=1,i+1
            x=x-1
            if(istriangular(a(n))) spiral(x,y)="*"
            n=n+1
        end do

        do j=1,i+1
            y=y+1
            if(istriangular(a(n))) spiral(x,y)="*"
            n=n+1
        end do
    end do

    do j = 1, dimn-1
        x = x + 1
        if(istriangular(a(n))) spiral(x, y) = "*"
        n = n + 1
    end do

    write(sstr, "(i0)") dimn
    fmt = "(" // sstr // "(a,1x))"
    do i = 1, dimn
        write(*,fmt) spiral(:, i)
    end do

    deallocate(a)

    contains

    function istriangular(num)
    logical::istriangular
    integer,intent(in)::num
    integer::m,temp,temp1

    temp=8*num+1
    temp1=int(sqrt(real(temp)))
    m=temp1**2

        if(m==temp)then
            istriangular=.true.
        else
            istriangular=.false.
        end if
    end function
end program
