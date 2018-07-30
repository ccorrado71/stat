   program statistic
   use stat
   real, dimension(1000) :: x
   real                  :: ave,var
!
   call random_number(x)
   call avevar(x,ave,var)
   write(0,'(a,f10.3,a,f10.3)')'Average: ',ave,' Variance: ',var
!
   end program statistic
