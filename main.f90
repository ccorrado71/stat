   program statistic
   use stat
   integer, parameter     :: NDATA = 10000
   real, dimension(NDATA) :: x
   real                   :: ave,var
!
   call random_number(x)
   call avevar(x,ave,var)
   write(0,*)'Average: ',ave,' Variance: ',var   ! da sistemare
!
   end program statistic
