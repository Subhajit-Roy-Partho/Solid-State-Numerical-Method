program Hofstadter
	implicit none !all variables needed to be defined


	!variable decleration
	real :: nu=0,alpha=0,e=0.0,res=0.0,p2,q2,e_step = 0.01
	integer :: qmax=50,countAlpha=0,p,q,i,countE=0,m=1,j
	real, dimension(2,2) :: a,b,mult !2d matrix for array multiplication
	real, dimension(100000) :: alpha_mat !matrix dimension unknown so taking a long one
	logical :: br
	REAL, PARAMETER :: M_PI = 3.1415927 !Approximate value of pi


    print *, "Please enter maximum value of qmax"!Querying for maximum q value
    read (*,*) qmax!maximum q value input from user
    call system('rm gnuOut.txt')!removing gnuOut.txt file if present already
    open(1, file="gnuOut.txt",status="new")!opening gnuOut.txt for storing all the alpha value in gnuplot format


    !****** Main Itteration starting here **********

    do q = 1,qmax !denominator q values
    	do p = 0,q !numerator p values
    		br =.TRUE. !for checking the overlapping condition
    		p2 = p*1.0000 !using p and q were giving some errors so taking a real version of them
    		q2 = q*1.0000
    		alpha = p2/q2 !alpha value calculated here
    		do i = 1, countAlpha!to prevent repetation
    			if(alpha == alpha_mat(i)) then
    				br= .FALSE.!if repeated then next if condition will not proceed
    				EXIT !Exiting when found
    			end if
    		end do
    		if(br) then!continue if alpha value not repeated
    			countAlpha = countAlpha +1 !Alpha count used for indexing the container of all the alpha values required to check overcounting
     			alpha_mat(countAlpha)=alpha!Storing all the alpha values 
     			nu = M_PI/(2.0*q)!nu value
     			e=-4.0!starting e value
     			do while(e<=4.0)!itterating over all the e values with increament 0.01
     				m = 1! m value starting form 1 with 1 incrementation

     				!matrix Initialization
     				a(1,1) = e -2*cos(2*M_PI*m*alpha-nu)
     				a(1,2) = -1.0
     				a(2,1) = 1.0
     				a(2,2) = 0.0

     				b(1,1) = e -2*cos(2*M_PI*(m+1)*alpha - nu)
     				b(1,2) = -1.0
     				b(2,1) = 1.0
     				b(2,2) = 0.0

     				do j=1,q-1 !The loop has to only run q-1 times as we have already have A(m=1) and only have to multiply A(m=q)...A(m=2) with it.
     					!setting m = 0
     					mult(1,1) = 0.0
     					mult(1,2) = 0.0
     					mult(2,1) = 0.0
     					mult(2,2) = 0.0

     					call matrixMultiplication(a,b,mult)!a*b =mult matrix operation
     					call equateMatrix(a,mult)! a= mult matrix operation
     					m = m+1
     					b(1,1)= e -2*cos(2*M_PI*(m+1)*alpha - nu)

     				end do
     				call trace(a,res) !finding trace of a and storing it to res
     				if(ABS(res)<=4.0) then!if trace <=4 then e and alpha are written down
     					write(1,*) e, alpha
     				end if
     				e = e+e_step!Incrementation in e
     			end do

    		end if
    	end do
    end do

    print*, "Total number of alpha components are:", countAlpha!Total alpha count displayed
    
    !**** Plotting it using gnuplot *****
    call system('rm gnuCommands.txt')
    open(2, file="gnuCommands.txt",status="new")
    write(2,*) "set key off"
    write(2,*) "set xlabel '{/Symbol(e)}'"
    write(2,*) "set ylabel '{/Symbol(a)}'"
    write(2,*) "plot 'gnuOut.txt' w p pt 7 ps 0.05"
    call system("gnuplot gnuCommands.txt --persist")

    close(2)!closing opened files
    close(1)
end program Hofstadter


! For finding trace of a 2-d matrix
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


!a*b=mult operation
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


!a=mult Matrix Operation
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
