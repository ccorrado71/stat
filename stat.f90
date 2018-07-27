 MODULE STAT

 implicit none

 type stat_type
   real :: ave
   real :: var
 end type stat_type

 CONTAINS

   subroutine avevar(data,ave,var)
!
!  Calcola media e varianza
!
   real, dimension(:), intent(in) :: data
   real, intent(out)              :: ave,var
   integer                        :: n
   real, dimension(size(data))    :: s
!
   n=size(data)
   ave=sum(data(:))/n
   s(:)=data(:)-ave
   var=dot_product(s,s)
   var=(var-sum(s)**2/n)/(n-1)
!
   end subroutine avevar

  !--------------------------------------------------------------------

   subroutine avestd(data,ans,avef,nd)
!
!  Calcola la media eliminando i dati con |data(i) - ave| > ans*std
!
   real, dimension(:), intent(in) :: data
   real, intent(in)               :: ans
   real, intent(out)              :: avef
   integer, intent(out), optional :: nd
   real                           :: avei,var,stdn
   integer                        :: i,ni,nf
!
   call avevar(data,avei,var)
   stdn = ans*sqrt(var)
!
   ni = size(data)
   nf = 0
   avef = 0
   do i=1,ni
      if(abs(data(i) - avei) <= stdn) then
         avef = avef + data(i)
         nf = nf + 1
      endif
   enddo
   if (nf == 0) then  ! nf=0 se i dati sono tutti uguali e var = 0
       avef = avei
   else
       avef = avef / nf
   endif
   if (present(nd)) then
       nd = ni - nf
   endif
!
   end subroutine avestd

  !------------------------------------------------------------------------------------------

   real function Rvalue(fden,fval)
   real, dimension(:), intent(in) :: fden  ! grandezza a denominatore
   real, dimension(:), intent(in) :: fval
   real                           :: scaleR
   real                           :: sumFo
!
   sumFo = sum(fden)
   scaleR = sumFo / sum(fval)
   Rvalue = (sum(abs(fden-scaleR*fval)) / sumFo)*100.
!
   end function Rvalue

 END MODULE STAT
