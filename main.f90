   program statistic
   use stat
   integer, parameter     :: NDATA = 1000
   real, dimension(NDATA) :: x
   real                   :: ave,var
!
   call random_number(x)
   call avevar(x,ave,var)
   write(0,*)'Average: ',ave,' Variance: ',var
!
   end program statistic
