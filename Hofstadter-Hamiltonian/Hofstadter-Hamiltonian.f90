program Hofstadter
	implicit none

	real :: nu=0,alpha=0,e=0.0,res=0.0,p2,q2,e_step = 0.01
	integer :: qmax=50, dimx=800,countAlpha=0,p,q,i,countE=0,m=1,j
	real, dimension(2,2) :: a,b,mult
	real, dimension(100000) :: alpha_mat
	logical :: br
	REAL, PARAMETER :: M_PI = 3.1415927

    print *, "Please enter maximum value of qmax"
    read (*,*) qmax
    call system('rm gnuOut.txt')
    open(1, file="gnuOut.txt",status="new")

    do q = 1,qmax
    	do p = 0,q
    		br =.TRUE.
    		p2 = p*1.0000
    		q2 = q*1.0000
    		alpha = p2/q2
    		do i = 1, countAlpha
    			if(alpha == alpha_mat(i)) then
    				br= .FALSE.
    				EXIT
    			end if
    		end do
    		if(br) then
    			countAlpha = countAlpha +1
     			alpha_mat(countAlpha)=alpha
    			countE=0
     			nu = M_PI/(2.0*q)
     			e=-4.0
     			do while(e<=4.0)
     				m = 1
     				a(1,1) = e -2*cos(2*M_PI*m*alpha-nu)
     				a(1,2) = -1.0
     				a(2,1) = 1.0
     				a(2,2) = 0.0

     				b(1,1) = e -2*cos(2*M_PI*(m+1)*alpha - nu)
     				b(1,2) = -1.0
     				b(2,1) = 1.0
     				b(2,2) = 0.0

     				do j=1,q-1 !The loop has to only run q-1 times as we have already have A(m=1) and only have to multiply A(m=q)...A(m=2) with it.
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
     				e = e+e_step
     			end do

    		end if
    	end do
    end do

    print*, "Total number of alpha components are:", countAlpha
    
    call system('rm gnuCommands.txt')
    open(2, file="gnuCommands.txt",status="new")
    write(2,*) "set key off"
    write(2,*) "set xlabel 'e'"
    write(2,*) "set ylabel 'alpha'"
    write(2,*) "plot 'gnuOut.txt' w p pt 7 ps 0.05"
    call system("gnuplot gnuCommands.txt --persist")

    close(2)
    close(1)
end program Hofstadter


SUBROUTINE trace(a,res)

implicit none

real, dimension(2,2) :: a
real :: res
integer :: i
res =0
do i=1,2
	res = res + a(i,i)
end do

end SUBROUTINE trace

SUBROUTINE matrixMultiplication(a,b,mult)

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


SUBROUTINE equateMatrix(a,mult)
implicit none

real, dimension(2,2) :: a,mult
integer :: i,j

do i =1,2
	do j=1,2
		a(i,j) = mult(i,j)
	end do
end do

end SUBROUTINE equateMatrix
