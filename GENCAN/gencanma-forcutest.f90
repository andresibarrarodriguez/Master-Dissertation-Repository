! *****************************************************************
! *****************************************************************

program gencanma

  use bmgencan, only: gencan, genunc
  use iso_c_binding, only: c_ptr, c_loc, c_f_pointer

  implicit none

  type pdata_type
     integer :: n_orig,nbds,nfree
     integer :: counters(3) = 0
     logical, allocatable :: free(:),frelem(:)
     integer, allocatable :: freeind(:),ifrelem(:),renumbered(:)
     real(kind=8), allocatable :: gfull(:),xfull(:)
  end type pdata_type

  ! LOCAL SCALARS
  logical :: extallowed,hfixstr
  integer :: allocerr,hnnzmax,i,ierr,istop,iter,maxit,n,nargs,nfnoprogrmax,status
  real(kind=8) :: beta,eps,eta,f,finish,ftarget,gpsupn,r,sgmmult,start
  type(pdata_type), target :: pdata

  ! LOCAL ARRAYS
  character(len=10) :: pname
  character(len=100) :: arg
  logical, allocatable :: lind(:),uind(:)
  integer :: maxcountg(4)
  real(kind=8), allocatable :: g(:),lbnd(:),lbnd_orig(:),ubnd(:),ubnd_orig(:),x(:)

  open(10,file='OUTSDIF.d',form='formatted',status='old')

  rewind 10

  call cutest_udimen(status,10,pdata%n_orig)
  
  if ( status .ne. 0 ) then
     write(*,*) 'cutest_udimen: status not equal to zero. status = ',status
     stop
  end if

  allocate(lbnd_orig(pdata%n_orig),ubnd_orig(pdata%n_orig),pdata%xfull(pdata%n_orig), &
       pdata%gfull(pdata%n_orig),pdata%free(pdata%n_orig),pdata%renumbered(pdata%n_orig), &
       stat=allocerr)
  
  if ( allocerr .ne. 0 ) then
     write(*,*) 'Allocation error.'
     stop
  end if
  
  call cutest_usetup(status,10,6,20,pdata%n_orig,pdata%xfull,lbnd_orig,ubnd_orig)
  
  if ( status .ne. 0 ) then
     write(*,*) 'cutest_usetup: status not equal to zero. status = ',status
     stop
  end if

  where ( lbnd_orig(1:pdata%n_orig) .eq. ubnd_orig(1:pdata%n_orig) )
     pdata%free(1:pdata%n_orig) = .false.
     pdata%xfull(1:pdata%n_orig) = lbnd_orig(1:pdata%n_orig)
  elsewhere
     pdata%free(1:pdata%n_orig) = .true.
  end where

  pdata%nfree = count( pdata%free(1:pdata%n_orig) )

  allocate(lind(pdata%nfree),lbnd(pdata%nfree),uind(pdata%nfree),ubnd(pdata%nfree), &
       g(pdata%nfree),x(pdata%nfree),pdata%freeind(pdata%nfree),stat=allocerr)
  
  if ( allocerr .ne. 0 ) then
     write(*,*) 'Allocation error.'
     stop
  end if

  pdata%freeind(1:pdata%nfree) = pack( (/ (i,i=1,pdata%n_orig) /), pdata%free(1:pdata%n_orig) )
  pdata%renumbered(1:pdata%n_orig) = unpack( (/ (i,i=1,pdata%nfree) /), pdata%free(1:pdata%n_orig), 0 )

  n = pdata%nfree

  x(1:n) = pdata%xfull(pdata%freeind(1:pdata%nfree))
  lbnd(1:n) = lbnd_orig(pdata%freeind(1:pdata%nfree))
  ubnd(1:n) = ubnd_orig(pdata%freeind(1:pdata%nfree))
  
  where( lbnd(1:n) .gt. - 1.0d+20 )
     lind(1:n) = .true.
  elsewhere
     lind(1:n) = .false.
  end where

  where( ubnd(1:n) .lt.   1.0d+20 )
     uind(1:n) = .true.
  elsewhere
     uind(1:n) = .false.
  end where

  pdata%nbds = count( lind(1:n) ) + count( uind(1:n) )
  
  deallocate(lbnd_orig,ubnd_orig,stat=allocerr)

  if ( allocerr .ne. 0 ) then
     write(*,*) 'Dellocation error.'
     stop
  end if
  
  call cutest_udimsh(status,hnnzmax)
  
  if ( status .ne. 0 ) then
     write(*,*) 'cutest_udimsh: status not equal to zero. status = ',status
     stop
  end if

  hnnzmax = hnnzmax + n
  
  allocate(pdata%frelem(hnnzmax),pdata%ifrelem(hnnzmax),stat=allocerr)

  if ( allocerr .ne. 0 ) then
     write(*,*) 'Allocation error.'
     stop
  end if
  
  call cutest_pname(status,10,pname)

  if ( status .ne. 0 ) then
     write(*,*) 'cutest_pname: status not equal to zero. status = ',status
     stop
  end if

  close(10)

  ! Set default values for parameters
  beta            =    0.5d0
  eta             =    1.0d+04
  r               =    0.1d0
  sgmmult         =   10.0d0
  nfnoprogrmax    =        3
  maxcountg(1:4)  = (/ 1, 100, 5000, 10000 /)
  extallowed      =   .true.
  
  ! Read command-line arguments if provided
  nargs = command_argument_count()
  
  if ( nargs .ge. 1 ) then
     call get_command_argument(1, arg)
     read(arg,*) beta
  end if
  
  if ( nargs .ge. 2 ) then
     call get_command_argument(2, arg)
     read(arg,*) eta
  end if
  
  if ( nargs .ge. 3 ) then
     call get_command_argument(3, arg)
     read(arg,*) r
  end if
  
  if ( nargs .ge. 4 ) then
     call get_command_argument(4, arg)
     read(arg,*) sgmmult
  end if
  
  if ( nargs .ge. 5 ) then
     call get_command_argument(5, arg)
     read(arg,*) nfnoprogrmax
  end if
  
  if ( nargs .ge. 6 ) then
     call get_command_argument(6, arg)
     read(arg,*) maxcountg(1)
  end if
  
  if ( nargs .ge. 7 ) then
     call get_command_argument(7, arg)
     read(arg,*) maxcountg(2)
  end if
  
  if ( nargs .ge. 8 ) then
     call get_command_argument(8, arg)
     read(arg,*) maxcountg(3)
  end if
  
  if ( nargs .ge. 9 ) then
     call get_command_argument(9, arg)
     read(arg,*) maxcountg(4)
  end if

  if ( nargs .ge. 10) then
       call get_command_argument(10, arg)
       read(arg,*) extallowed
   end if

  ftarget     =  - 1.0d+12
  eps         =    1.0d-08
  maxit       =      50000
  hfixstr     =     .true.

  call cpu_time(start)

  if ( pdata%nbds .eq. 0 ) then
     write(*,*) 'The problem is unconstrained.'
     call genunc(evalf,evalg,evalh,hnnzmax,hfixstr,n,x,f,g,gpsupn, &
          ftarget,eps,maxit,extallowed,nfnoprogrmax,beta,eta,sgmmult,maxcountg, &
          iter,ierr,istop,tabline=tabline,pdata=c_loc(pdata))
     
  else
     write(*,*) 'The problem is a bound constrained problem.'
     call gencan(evalf,evalg,evalh,hnnzmax,hfixstr,n,x,lind,lbnd, &
          uind,ubnd,f,g,gpsupn,ftarget,eps,maxit,extallowed,nfnoprogrmax, &
          beta,eta,r,sgmmult,maxcountg,iter,ierr,istop,tabline=tabline, &
          pdata=c_loc(pdata))
  end if
  
  call cpu_time(finish)

  !write(*,*)
  !write(*,*) 'Problem name                                          = ',pname
  !write(*,*) 'Parameters to be tuned'
  !write(*,*) 'beta            = ', beta
  !write(*,*) 'eta             = ', eta
  !write(*,*) 'r               = ', r
  !write(*,*) 'sgmmult         = ', sgmmult
  !write(*,*) 'nfnoprogrmax    = ', nfnoprogrmax
  !write(*,*) 'maxcountg(1:4)  = ', maxcountg(1:4)
  !write(*,*) 'extallowed      = ', extallowed
  write(*,*) 'Problem name                                          = ',pname
  write(*,*) '(CUTEst formulation) Number of variables              = ',pdata%n_orig
  
  write(*,*)
  write(*,*) '(Problem effectively tackled) Number of variables     = ',n
  write(*,*) '(Problem effectively tackled) Number of bounds        = ',pdata%nbds

  write(*,*)
  write(*,*) '(REPORTED BY SOLVER) istop                            = ',istop
  write(*,*) '(REPORTED BY SOLVER) ierr                             = ',ierr
  write(*,*) '(REPORTED BY SOLVER) f                                = ',f
  write(*,*) '(REPORTED BY SOLVER) gpsupn                           = ',gpsupn
  write(*,*) '(REPORTED BY SOLVER) Number of iterations             = ',iter
  write(*,*)
  write(*,*) '(COMPUTED BY CALLER) Number of function evaluations   = ',pdata%counters(1)
  write(*,*) '(COMPUTED BY CALLER) Number of gradient evaluations   = ',pdata%counters(2)
  write(*,*) '(COMPUTED BY CALLER) Number of Hessian  evaluations   = ',pdata%counters(3)
  write(*,*) '(COMPUTED BY CALLER) CPU time in seconds              = ',finish - start

  ! *****************************************************************
  ! *****************************************************************
  ! Just checking ...
  pdata%xfull(pdata%freeind(1:pdata%nfree)) = x(1:n)
    
  call cutest_ufn(status,pdata%n_orig,pdata%xfull,f)
  
  if ( status .ne. 0 ) then
     write(*,*) 'error when calling cutest_ufn in the main file.'
     stop
  end if
  
  write(*,*)
  write(*,*) '(COMPUTED BY CALLER) f                                = ',f

  write(*,*)
  write(*,*) 'When a quantity appears as computed by solver and computed by caller, they must coincide.'
  write(*,*) '(In case they do not coincide, please report it as a bug.)'
  ! *****************************************************************
  ! *****************************************************************
  
  open(20,file='tabline.txt')
  write(20,9000) istop,ierr,finish - start,pdata%n_orig,n,pdata%nbds,f,gpsupn,iter,pdata%counters(1:3)
  close(20)
  
  deallocate(g,lind,lbnd,uind,ubnd,x,pdata%freeind,pdata%xfull,pdata%gfull,pdata%free,pdata%renumbered, &
       pdata%frelem,pdata%ifrelem,stat=allocerr)
  if ( allocerr .ne. 0 ) then
     write(*,*) 'Deallocation error.'
     stop
  end if

  stop

9000 format(1X,I2,1X,I3,0P,F12.6,3(1X,I6),1X,1P,D24.16,1X,1P,D7.1,4(1X,I8))

contains
  
  ! *****************************************************************
  ! *****************************************************************

  subroutine evalf(n,x,f,inform,pdataptr)

    implicit none

    ! SCALAR ARGUMENTS
    integer, intent(in) :: n
    integer, intent(inout) :: inform
    real(kind=8), intent(out) :: f
    type(c_ptr), optional, intent(in) :: pdataptr

    ! ARRAY ARGUMENTS
    real(kind=8), intent(in) :: x(n)
    
    ! LOCAL SCALARS
    integer :: status
    type(pdata_type), pointer :: pdata
    
    call c_f_pointer(pdataptr, pdata)
    pdata%counters(1) = pdata%counters(1) + 1
    
    pdata%xfull(pdata%freeind(1:pdata%nfree)) = x(1:n)
    
    call cutest_ufn(status,pdata%n_orig,pdata%xfull,f)
    
    if ( status .ne. 0 ) then
       inform = -91
       return
    end if
    
  end subroutine evalf

  ! *****************************************************************
  ! *****************************************************************

  subroutine evalg(n,x,g,inform,pdataptr)

    implicit none

    ! SCALAR ARGUMENTS
    integer, intent(in) :: n
    integer, intent(inout) :: inform
    type(c_ptr), optional, intent(in) :: pdataptr
  
    ! ARRAY ARGUMENTS
    real(kind=8), intent(in) :: x(n)
    real(kind=8), intent(out) :: g(n)
    
    ! LOCAL SCALARS
    integer :: status
    type(pdata_type), pointer :: pdata
    
    call c_f_pointer(pdataptr, pdata)
    pdata%counters(2) = pdata%counters(2) + 1
    
    pdata%xfull(pdata%freeind(1:pdata%nfree)) = x(1:n)
    
    call cutest_ugr(status,pdata%n_orig,pdata%xfull,pdata%gfull)
    
    if ( status .ne. 0 ) then
       inform = -92
       return
    end if
    
    ! Remove elements related to fixed variables
    
    g(1:n) = pdata%gfull(pdata%freeind(1:pdata%nfree))

    ! Remove NaN
    
    where ( isnan( g(1:n) ) )
       g(1:n) = 0.0d0
    end where
    
  end subroutine evalg

  ! *****************************************************************
  ! *****************************************************************

  subroutine evalh(n,x,lim,hnnz,hrow,hcol,hval,inform,pdataptr)

    implicit none

    ! SCALAR ARGUMENTS
    integer, intent(in) :: lim,n
    integer, intent(inout) :: inform
    integer, intent(out) :: hnnz
    type(c_ptr), optional, intent(in) :: pdataptr

    ! ARRAY ARGUMENTS
    real(kind=8), intent(in) :: x(n)
    integer, intent(out) :: hcol(lim),hrow(lim)
    real(kind=8), intent(out) :: hval(lim)

    ! LOCAL SCALARS
    integer :: nfrelem,status
    type(pdata_type), pointer :: pdata

    call c_f_pointer(pdataptr, pdata)
    pdata%counters(3) = pdata%counters(3) + 1

    pdata%xfull(pdata%freeind(1:pdata%nfree)) = x(1:n)

    ! Upper triangle
    call cutest_ush(status,pdata%n_orig,pdata%xfull,hnnz,lim,hval,hrow,hcol)
    
    if ( status .ne. 0 ) then
       inform = -93
       return
    end if
    
    ! Remove elements related to fixed variables
    
    if ( pdata%nfree .ne. pdata%n_orig ) then
       pdata%frelem(1:hnnz) = pdata%free(hrow(1:hnnz)) .and. pdata%free(hcol(1:hnnz))
      
       nfrelem = count( pdata%frelem(1:hnnz) )
       pdata%ifrelem(1:nfrelem) = pack( (/ (i, i=1,hnnz) /), pdata%frelem(1:hnnz) )

       hnnz = nfrelem
       hrow(1:nfrelem) = pdata%renumbered(hrow(pdata%ifrelem(1:nfrelem)))
       hcol(1:nfrelem) = pdata%renumbered(hcol(pdata%ifrelem(1:nfrelem)))
       hval(1:nfrelem) = hval(pdata%ifrelem(1:nfrelem))
    end if

    ! Remove NaN
    
    where ( isnan( hval(1:hnnz) ) )
       hval(1:hnnz) = 0.0d0
    end where
    
  end subroutine evalh

  ! *****************************************************************
  ! *****************************************************************

  subroutine tabline(n,f,gpsupn,iter,pdataptr)
    
    implicit none

    ! SCALAR ARGUMENTS
    integer, intent(in) :: iter,n
    real(kind=8), intent(in) :: f,gpsupn
    type(c_ptr), optional, intent(in) :: pdataptr

    ! LOCAL SCALARS
    type(pdata_type), pointer :: pdata

    call c_f_pointer(pdataptr,pdata)

    open(20,file='solver-interrupted-tabline.txt')
    write(20,8000) pdata%n_orig,n,pdata%nbds,f,gpsupn,iter,pdata%counters(1:3)
    close(20)
    
8000 format(3(1X,I6),1X,1P,D24.16,1X,1P,D7.1,4(1X,I8))

  end subroutine tabline

end program gencanma
