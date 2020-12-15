program Hofstarder
implicit none

	real :: nu=0,alpha=0,e=0.0,res=0.0,p2,q2
	integer :: qmax=50, dimx=800,countAlpha=1,p,q,i,countE=0,m=1,j
	real, dimension(2,2) :: a,b,mult
	real, dimension(100000) :: alpha_mat
	logical :: br
	REAL, PARAMETER :: M_PI = 3.1415927

    print *, "Please enter maximum value of qmax"
    !read (*,*) qmax
    open(1, file="gnuOut.txt",status="old")
    !dimy=(qmax*(qmax+1))/2

    do q = 1,qmax
    	do p = 0,q
    		br =.TRUE.
    		p2 = p*1.0000
    		q2 = q*1.0000
    		alpha = p2/q2
    		do i = 1, countAlpha-3
    			if(alpha == alpha_mat(i)) then
    				br= .FALSE.
    				EXIT
    			end if
    		end do
    		if(br) then
    			countAlpha = countAlpha +1
     			alpha_mat(countAlpha-1)=alpha
    			countE=0
     			nu = M_PI/(2.0*q)
     			e=-4.0
     			do while(e<=4.0)
     				m=1
     				a(1,1)= e -2*cos(2*M_PI*m*alpha-nu)
     				a(1,2) = -1.0
     				a(2,1) = 1.0
     				a(2,2) = 0.0

     				b(1,1) = e -2*cos(2*M_PI*(m+1)*alpha - nu)
     				b(1,2) = -1.0
     				b(2,1) = 1.0
     				b(2,2) = 0.0

     				mult(1,1) = 0.0
     				mult(1,2) = 0.0
     				mult(2,1) = 0.0
     				mult(2,2) = 0.0

     				do j=1,q-1
     					mult(1,1) = 0.0
     					mult(1,2) = 0.0
     					mult(2,1) = 0.0
     					mult(2,2) = 0.0

     					call matrixMultiplication(a,b,mult)
     					call equateMatrix(a,mult)
     					m = m+1
     					b(1,1)= e -2*cos(2*M_PI*(m+1)*alpha - nu)

     				end do
     				call trace(a,res)
     				if(ABS(res)<=4.0) then
     					write(1,*) e, alpha
     				end if
     				e=e+0.1
     			end do

    		end if
    	end do
    end do

    print*, "Total number of alpha components are:", countAlpha


    close(1)
end program Hofstarder


! FUNCTION trace(a)
! implicit none

! real, dimension(2,2) :: a
! real :: result=0,trace
! integer :: i

! do i=1,2
! 	result = result+a(i,i)
! end do
! trace=result
! RETURN

! END FUNCTION trace


SUBROUTINE trace(a,res) !To find the trace of a 2x2 matrix

implicit none

real, dimension(2,2) :: a
real :: res
integer :: i
res =0
do i=1,2
	res = res + a(i,i)
end do

end SUBROUTINE trace

SUBROUTINE matrixMultiplication(a,b,mult) ! Matrix a*b = mult

implicit none

real, dimension(2,2) :: a,b,mult
integer :: i,j,k

do i=1,2
	do j=1,2
		do k=1,2
			mult(i,j) = mult(i,j) + a(i,k)*b(k,j)
		end do
	end do
end do

END SUBROUTINE matrixMultiplication


SUBROUTINE equateMatrix(a,mult) ! Makes Matrix a = mult
implicit none

real, dimension(2,2) :: a,mult
integer :: i,j

do i =1,2
	do j=1,2
		a(i,j) = mult(i,j)
	end do
end do

end SUBROUTINE equateMatrix