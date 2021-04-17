program transmission
    implicit none !Define Everything
    real :: a,K=9.0,lam=2.0,T,abs=100,lam2=4.0 !a = Incident Wave Vector; E = K^2; lam = \lambda; T = Transmission Amplitude; abs = Upper limit check for Transmission amplitude
    ! for which loop will terminate; lam2 = \lambda for 2nd peak
    integer :: i =180,M,j,Mmax=16 ! Mmax = Maximum number of matrix multiplication.
    complex :: cs,ct,z,z1,ctn,csn,cs2,ct2 !cs = Complex Resistance Amplitude; ct = complex inverse transmission amplitude, z = phase factor; z1 = complex conjugate of z
	!ct2, cs2 is for the 2nd barrier
	!ctn, csn are to keep continious count
    CALL SYSTEM("touch gnuscript.gnu") !creating the files in advance to prevent any error
    CALL SYSTEM("touch data.dat") 

    open(1,file="data.dat",status="old")!Opening file for writing data

    !!!!!!!!!!!!!!!! Main Program starts here !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    do j=1,1000
        K = Float(j)/100.0

        do i = 100, 500
            cs = complex(0,-1*lam/K)
            cs2 = complex(0,-1*lam2/K)
            ct = (1,0)-cs
            ct2 = (1,0) - cs2
            a = Float(i)/100.0
            z = CExp(complex(0,K*a))
            z1 = CExp(complex(0,-1*K*a))
            ctn = z1*ct2*ct2 + z*(CAbs(cs2))**2
            csn = cs2*(z*(ct2*complex(1,-1))+z1*ct2)
            do M = 1, Mmax
		    if (MOD(M,2) == 1) then
		        ctn = z1*ctn*ct + z*csn*cs
		        csn = z*cs*ctn + z1*csn*ct
		        T = real(CAbs(ctn))
		        if ( T>abs) then !checking the value of transmission cofficient if more than abs then the solution is unbounded and exit the loop
		            ! write(1,*) K,a
		            exit
		        end if
		        if ( M==Mmax ) then
		            write(1,*) K,a
		        end if
		    end if
		    if(MOD(M,2)==0) then
		    	ctn = z1*ctn*ct2 + z*csn*cs2
		        csn = z*cs2*ctn + z1*csn*ct2
		        T = real(CAbs(ctn))
		        if ( T>abs) then !checking the value of transmission cofficient if more than abs then the solution is unbounded and exit the loop
		            ! write(1,*) K,a
		            exit
		        end if
		        if ( M==Mmax ) then
		            write(1,*) K,a
		        end if
		    end if
            end do
        end do
    end do

    !!!!!!!!!!!!!!! Main Program Ends Here !!!!!!!!!!!!!!!!!!!!!!!!!!

    close(1)

    !!!!!!!!!!!!!!! Gnuplot graphing starts here !!!!!!!!!!!!!!!!!!!!

    open(2,file="gnuscript.gnu",status="old") ! gnuscipt for plotting

    ! character :: filewrite = 'plot "data.dat" w p pt 7 ps 0.01 \n set xlabel "K" \n set ylabel "Incident wavevector a" \n set title "Plot of the bands using Kronig-Penney model"'
    ! ! filewrite = '

    write(2,*) "plot 'data.dat' w p pt 7 ps 0.1"
    write(2,*) "set xlabel 'K'"
    write(2,*) "set ylabel 'Incident wavevector a'"
    write(2,*) "set title 'Plot of the bands using Kronig-Penney model'"
    write(2,*) "pause 30" !To keep the interactive window
    close(2)
    
    CALL SYSTEM("gnuplot -persist gnuscript.gnu")
end program transmission