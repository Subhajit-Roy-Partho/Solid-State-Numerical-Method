program transmission
    implicit none !Define Everything
    real :: a,K=9.0,lam=2.0,T,abs=100 !a = Incident Wave Vector; E = K^2; lam = \lambda; T = Transmission Amplitude; abs = Upper limit check for Transmission amplitude
    ! for which loop will terminate
    integer :: i =180,M,j,Mmax=20 ! Mmax = Maximum number of matrix multiplication.
    complex :: cs,ct,z,z1 !cs = Complex Resistance Amplitude; ct = complex inverse transmission amplitude, z = phase factor; z1 = complex conjugate of z

    CALL SYSTEM("touch gnuscript.gnu") !creating the files in advance to prevent any error
    CALL SYSTEM("touch data.dat") 

    open(1,file="data.dat",status="old")!Opening file for writing data

    !!!!!!!!!!!!!!!! Main Program starts here !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    do j=1,10000
        K = Float(j)/1000.0

        do i = 1000, 5000
            cs = complex(0,-1*lam/K)
            ct = (1,0)-cs
            a = Float(i)/1000.0
            z = CExp(complex(0,K*a))
            z1 = CExp(complex(0,-1*K*a))
            do M = 1, Mmax
                ct = z1*ct*ct + z*(CAbs(cs))**2
                cs = cs*(z*(ct*complex(1,-1))+z1*ct)
                T = real(CAbs(ct))
                if ( T>abs) then !checking the value of transmission cofficient if more than abs then the solution is unbounded and exit the loop
                    ! write(1,*) K,a
                    exit
                end if
                if ( M==Mmax ) then
                    write(1,*) K,a
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

    write(2,*) "plot 'data.dat' w p pt 7 ps 0.01"
    write(2,*) "set xlabel 'K'"
    write(2,*) "set ylabel 'Incident wavevector a'"
    write(2,*) "set title 'Plot of the bands using Kronig-Penney model'"
    write(2,*) "pause 30" !To keep the interactive window
    close(2)
    
    CALL SYSTEM("gnuplot -persist gnuscript.gnu")
end program transmission