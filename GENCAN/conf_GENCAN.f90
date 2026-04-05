program minf
    use problem_control
    implicit none
    
    ! SCALAR ARGUMENTS
    integer :: n, m   
    real(kind=8) :: f               ! Value of the objective function,
    real(kind=8) :: cnorm           ! Norm of constraints,
    real(kind=8) :: snorm           ! Norm of complementarity,
    real(kind=8) :: nlpsupn         ! Norm of the projected gradient,
    integer :: inform               ! Output code.
    
    ! COMMAND LINE ARGUMENTS
    integer :: problem_id
    character(len=20) :: method_str
    character(len=50) :: method_token
    
    ! ALGENCAN PARAMETERS
    logical :: checkder, coded(11)
    integer :: jcnnzmax, hnnzmax, nvparam, allocerr
    real(kind=8) :: epsfeas, epsopt, efstain, eostain, efacc, eoacc
    character(len=80) :: outputfnm, specfnm, vparam(10)
    

    ! ARRAY ARGUMENTS
    logical, allocatable :: equatn(:), linear(:)
    real(kind=8), allocatable :: lambda(:), x(:), l(:), u(:)
    
    
    ! EXTERNAL SUBROUTINES DECLARATIONS
    external :: myevalf, myevalg, myevalh, myevalc, myevaljac, myevalhc
    external :: myevalfc, myevalgjac, myevalgjacp, myevalhl, myevalhlp
    
    call get_command_argument(1, method_str)
    read(method_str, *) problem_id
    call get_command_argument(2, method_str)
 
    !current_nprob = problem_id
    
    ! Map the internal minimization method to be used by GENCAN 3.1.1.
    ! The methods available in this version are:
    ! Newton with line search,
    ! truncated Newton with line search, and
    ! Newton with trust-region globalization.
    select case (trim(method_str))
        case ('newton')
            method_token = 'NEWTON-LINE-SEARCH-INNER-SOLVER'
        case ('tn')
            method_token = 'TRUNCATED-NEWTON-LINE-SEARCH-INNER-SOLVER'
        case ('tr')
            method_token = 'TRUST-REGIONS-INNER-SOLVER'
    end select
    call getdim(n,m)
    
    ! ALLOCATE MEMORY FOR CONSTRAINTS, (In case of problems with general constraints)
    allocate(equatn(max(m,1)), linear(max(m,1)), lambda(max(m,1)), x(n), l(n), u(n), stat=allocerr)

    ! SET THE INITIAL POINT 
    call getx0(n,x,l,u)
    
    ! SET THE ALGENCAN PARAMETERS
    jcnnzmax = 100     ! Estimate of the maximum number of nonzero elements of the Jacobian of the constraints,
    hnnzmax = 100      ! Estimate of the maximum number of nonzero elements of the Hessian,
    epsfeas = 1.0d-8   ! Tolerance for declaring convergence of feasibility,
    epsopt = 1.0d-8    ! Tolerance for declaring convergence of optimality,
    efstain = 1.0d-4   ! Tolerance for declaring convergence to an infeasible point,
    eostain = 1.0d-12  ! Tolerance for declaring convergence to an infeasible point,
    efacc = 1.0d-4     ! Define an acceleration parameter,
    eoacc = 1.0d-4     ! Define an acceleration parameter.,
    outputfnm = 'algencan_solution.out'   ! output file, 
    specfnm = ''       ! Specification file,
    nvparam = 2        ! Number of additional parameters,
    vparam(1) = 'ITERATIONS-OUTPUT-DETAIL 11'
    vparam(2) = trim(method_token)	
    checkder = .false. ! We do not verify derivatives during tuning,
    coded(1:2) = .true.   ! fsub, gsub are codificated,
    coded(3) = .true.    ! hsub is codificated,
    coded(4:11) = .false. ! fcsub, gjacsub, gjacpsub, hlsub, hlpsub are not codificated.
    
    call algencan(myevalf,myevalg,myevalh,myevalc,myevaljac,myevalhc, &
                  myevalfc,myevalgjac,myevalgjacp,myevalhl,myevalhlp, &
                  jcnnzmax,hnnzmax,epsfeas,epsopt,efstain,eostain,efacc,eoacc, &
                  outputfnm,specfnm,nvparam,vparam,n,x,l,u,m,lambda,equatn, &
                  linear,coded,checkder,f,cnorm,snorm,nlpsupn,inform)
    

    !write(*,'(A,*(1X,E15.8))') 'x =', x 
    
   write(*,'(A,1X,ES15.8)') 'fbest =', f
   
    !Deallocate memory.
    deallocate(equatn, linear, lambda,x,l,u, stat=allocerr)
    

end program minf
