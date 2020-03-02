!jei hou lets gou
program dynamics

use def_variables
use read_input
use init
use integration
use stadistics
use gdr_funcs

call cpu_time(start)
call open_input()
call read_parameters()
!call setr1279(seed)
!call srand(seed)

open(unit=un_mag,file='results.log')
write(unit=un_mag, fmt=*) '# Time         Temp      Kin     Potencial       E_tot        Pressure'
call open_input()
call read_parameters()

call allocate_arrays(Nsteps, Nprint, Npart)
call initialize()

do i=1,Nterm
        call vverlet()
        call andersen_termo()
enddo

call gdr_step()
call results()

do step =1,int(Nsteps/Nprint)
        time = time + dt
        do step_print=1, Nprint
                call vverlet()
                call andersen_termo()
        enddo
        call gdr_step()
        call results()
enddo
close(un_mag)

call gdr_final()
call statistics()
call cpu_time(finish)
print*,'CPU time:',finish-start,'s'

end program dynamics
