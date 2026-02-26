program montecarlo_search
   use problem_control
  implicit none

  ! LOCAL SCALARS
  integer :: allocerr,boxtype,i,itrial,itrialmax,n,m, nprob
  real(kind=8) :: f,fbest,seed
  character(len=32) :: arg

  ! LOCAL ARRAYS
  real(kind=8), allocatable :: x(:),xbest(:),l(:),u(:)

  ! EXTERNAL FUNCTIONS
  real(kind=8), external :: drand

  ! Read arguments from the terminal
  call get_command_argument(1, arg)
  read(arg,*) nprob
  call get_command_argument(2, arg)
  read(arg,*) boxtype
  call get_command_argument(3, arg)
  read(arg,*) itrialmax

  current_nprob = nprob
  call getdim(n,m)


  allocate(x(n),xbest(n),l(n),u(n),stat=allocerr)
  if ( allocerr .ne. 0 ) then
     write(*,*) 'Allocation error.'
     stop
  end if

  call getbounds(boxtype,n,l,u)
  seed = 123456.0d0
  fbest = huge( 1.0d0 )

  do itrial = 1,itrialmax
     do i = 1,n
        x(i) = l(i) + ( u(i) - l(i) ) * drand(seed)
     end do
     call evalf(n,x,f)
     if ( f .lt. fbest ) then
        fbest = f
        xbest(1:n) = x(1:n)
     end if
  end do

  write(*,'(A,E16.8)') 'fbest = ', fbest
  write(*,*) 'xbest = ', xbest(1:n)

  deallocate(x,xbest,l,u,stat=allocerr)
  if ( allocerr .ne. 0 ) then
     write(*,*) 'Deallocation error.'
     stop
  end if

end program montecarlo_search

! ******************************************************************
! ******************************************************************

! ******************************************************************
! ******************************************************************

subroutine getbounds(boxtype,n,l,u)
  implicit none

  ! SCALAR ARGUMENTS
  integer, intent(in) :: boxtype,n

  ! ARRAY ARGUMENTS
  real(kind=8), intent(out) :: l(n),u(n)

  if ( boxtype .eq. 1 ) then
     l(1:n) = - 1.0d0
     u(1:n) =   1.0d0
  else if ( boxtype .eq. 2 ) then
     l(1:n) = - 3.0d0
     u(1:n) =   3.0d0
  else if ( boxtype .eq. 3 ) then
     l(1:n) = - 5.0d0
     u(1:n) =   5.0d0
  else
     write(*,*) 'Undefined box type!'
     stop
  end if
end subroutine getbounds

! ******************************************************************
! ******************************************************************

subroutine evalf(n,x,f)
   use problem_control
   implicit none
   
    ! SCALAR ARGUMENTS
    integer, intent(in) :: n
    integer :: flag
    real(kind=8), intent(out) :: f
    ! ARRAY ARGUMENTS
    real(kind=8), intent(in) :: x(n)
   ! SELECTING THE OBJETIVE FUNCTION
    ! COMPUTE THE OBJETVE FUNCTION
    call myevalf(n, x, f, flag)
    flag = 0
end subroutine evalf



!subroutine evalf(nprob,n,x,f)
    !implicit none

    ! SCALAR ARGUMENTS
    !integer, intent(in) :: n,nprob
    !real(kind=8), intent(out) :: f

    ! ARRAY ARGUMENTS
    !real(kind=8), intent(in) :: x(n)

    ! LOCAL SCALARS
    !real(kind=4) :: fsp

    ! LOCAL ARRAYS
    !real(kind=4) :: xsp(n)

    !xsp(1:n) = x(1:n)
    !call OBJFCN(n,xsp,fsp,nprob)
    !f = fsp
!end subroutine evalf

! ******************************************************************
! ******************************************************************

function drand(ix)
  implicit none

  ! This is the random number generator of Schrage:
  !
  ! L. Schrage, A more portable Fortran random number generator, ACM
  ! Transactions on Mathematical Software 5 (1979), 132-138.

  ! FUNCTION TYPE
  real(kind=8) :: drand

  ! SCALAR ARGUMENT
  real(kind=8), intent(inout) :: ix

  ! LOCAL ARRAYS
  real(kind=8) :: a,p,b15,b16,xhi,xalo,leftlo,fhi,k

  data a/16807.d0/,b15/32768.d0/,b16/65536.d0/,p/2147483647.d0/

  xhi= ix/b16
  xhi= xhi - dmod(xhi,1.d0)
  xalo= (ix-xhi*b16)*a
  leftlo= xalo/b16
  leftlo= leftlo - dmod(leftlo,1.d0)
  fhi= xhi*a + leftlo
  k= fhi/b15
  k= k - dmod(k,1.d0)
  ix= (((xalo-leftlo*b16)-p)+(fhi-k*b15)*b16)+k
  if (ix.lt.0) ix= ix + p
  drand= ix*4.656612875d-10

  return
end function drand
