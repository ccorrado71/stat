   program statistic
   use stat
   real, dimension(1000) :: x
   real                  :: ave,var
!
   call random_number(x)
   call avevar(x,ave,var)
   write(0,*)'Average: ',ave,' Variance: ',var
!
   end program statistic
