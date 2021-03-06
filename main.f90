   program statistic
   use iso_fortran_env, only: stdout => OUTPUT_UNIT
   use stat
   implicit none
   integer, parameter     :: NDATA = 1000000
   real, dimension(NDATA) :: x
   real                   :: ave,var
   type(stat_type)        :: statt
   integer                :: i
!
   call random_number(x)
   x = 1.0
   x = x * 1000000000
!
   call avevar(x,ave,var)
   write(0,'(a,f20.6,a,f20.6)')'Average: ',ave,' Variance: ',var
!
   call statt%init()
   do i = 1, size(x)
      call statt%add(real(x(i),DP))
   enddo
   write(stdout,'(a,i0)')'N: ',statt%num()
   write(stdout,'(a,f20.6,a,f20.6)')'Average: ',statt%mean(),' Variance: ',statt%variance()
!
   end program statistic
