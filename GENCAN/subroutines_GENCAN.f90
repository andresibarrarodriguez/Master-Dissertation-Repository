! ----------------------------------------------------------------------
! MODULE FOR RELEATING THE PROBLEM WIH THEIR DIMENSION SIZE
! ----------------------------------------------------------------------
module problem_control
    implicit none
    integer :: current_nprob = 1
    !Here we define a vector with the dimensions according the table in the paper.
    integer, parameter :: ndims(18) = (/3,6,3,2,3,10,6,4,4,2,4,3,10,10,12,2,4,8/)
end module problem_control

! ----------------------------------------------------------------------
! SUBROUTINE FOR SETTING THE INITIAL DIMENSION OF THE FUNCTION
! ----------------------------------------------------------------------
subroutine getdim(n,m)
    use problem_control
    implicit none
    ! SCALAR ARGUMENTS
    integer, intent(out) :: m,n
    n = ndims(current_nprob)
    m = 0
end subroutine getdim


! ----------------------------------------------------------------------
! SUBROUTINE FOR SETTING THE INITIAL POINT
! ----------------------------------------------------------------------
subroutine getx0(n,x,l,u)
  use problem_control
  implicit none
  ! SCALAR ARGUMENTS
  integer, intent(in) :: n
  ! ARRAY ARGUMENTS
  real(kind=8), intent(out) :: x(n),l(n),u(n)
  real(kind=8), parameter :: factor = 1.0d0
  
  ! USE INITPT TO GET THE INITIAL POINT FOR CURRENT PROBLEM
  call INITPT(n, x, current_nprob, factor)
  
  l(1:n) = -1.0d20
  u(1:n) =  1.0d20
end subroutine getx0


! ----------------------------------------------------------------------
! SUBROUTINE TO EVALUATE THE OBJECTIVE FUNCTION USING OBJFCN
! ----------------------------------------------------------------------
subroutine myevalf(n,x,f,flag)
    use problem_control
    implicit none
    ! SCALAR ARGUMENTS
    integer, intent(in) :: n
    integer, intent(out) :: flag
    real(kind=8), intent(out) :: f
    ! ARRAY ARGUMENTS
    real(kind=8), intent(in) :: x(n)
   ! SELECTING THE OBJETIVE FUNCTION
    ! COMPUTE THE OBJETVE FUNCTION
    call OBJFCN(n, x, f, current_nprob)
    flag = 0
end subroutine myevalf

! -------------------------------------------------------------------------
! SUBROUTINE FOR EVALUATING THE GRADIENT VECTOR USING THE SUBROUTINE GRDFCN
! -------------------------------------------------------------------------
subroutine myevalg(n,x,g,flag)
    use problem_control
    implicit none
    !SCALARS ARGUMENTS
    integer, intent(in) :: n
    integer, intent(out) :: flag
    ! ARRAY ARGUMENTS
    real(kind=8), intent(in) :: x(n)
    real(kind=8), intent(out) :: g(n)
    ! SELECTING THE FUNCTION
    ! COMPUTE THE GRADIENT OF THE OBJETIVE FUNCTION
    call GRDFCN(n, x, g, current_nprob)
    flag = 0
end subroutine myevalg

! ----------------------------------------------------------------------
! SUBROUTINE FOR EVALUATING THE HESSIAN MATRIX USING THE SUBROUTINE HESFCN
! ----------------------------------------------------------------------
subroutine myevalh(n,x,hrow,hcol,hval,hnnz,lim,lmem,flag)
    use problem_control
    implicit none
    ! SCALARS ARGUMENTS
    integer, intent(in) :: n, lim
    logical, intent(out) :: lmem
    integer, intent(out) :: hnnz, flag
    integer :: i, j, idx
    ! ARRAY ARGUMENTS
    integer, intent(out) :: hrow(lim), hcol(lim)
    real(kind=8), intent(in) :: x(n)
    real(kind=8), intent(out) :: hval(lim)
    real(kind=8) :: hesd(n), hesl(n*(n-1)/2)
    ! SELECTING THE FUNCTION
    ! COMPUTING THE HESSIAN MATRIX
    call HESFCN(n, x, hesd, hesl, current_nprob)
    ! Lower Triangular Matrix (Diagonal)
    hnnz = 0
    do i=1,n
      hnnz = hnnz + 1
      hrow(hnnz) = i
      hcol(hnnz) = i
      hval(hnnz) = hesd(i)
    end do
    !  Lower Triangular Matrix (Strictly inferior elements) 
    idx = 0
    do i=2,n
      do j=1,i-1
        idx = idx + 1
        hnnz = hnnz + 1
        hrow(hnnz) = i
        hcol(hnnz) = j
        hval(hnnz) = hesl(idx)
      end do
    end do
    lmem = .false.
    flag = 0
end subroutine myevalh

! ----------------------------------------------------------------------
! SUBROUTINE FOR GENERAL CONSTRAINTS (If there exists)
! ----------------------------------------------------------------------
subroutine myevalc(n,x,ind,c,flag)
    integer, intent(in) :: n, ind
    real(kind=8), intent(in) :: x(n)
    real(kind=8), intent(out) :: c
    integer, intent(out) :: flag
    flag = 0
end subroutine myevalc

subroutine myevaljac(n,x,ind,jcvar,jcval,jcnnz,lim,lmem,flag)
    integer, intent(in) :: n, ind, lim
    real(kind=8), intent(in) :: x(n)
    logical, intent(out) :: lmem
    integer, intent(out) :: jcvar(lim), jcnnz, flag
    real(kind=8), intent(out) :: jcval(lim)
    jcnnz = 0
    lmem = .false.
    flag = 0
end subroutine myevaljac

subroutine myevalhc(n,x,ind,hclin,hccol,hcval,hcnnz,lim,lmem,flag)
    integer, intent(in) :: n, ind, lim
    real(kind=8), intent(in) :: x(n)
    logical, intent(out) :: lmem
    integer, intent(out) :: hclin(lim), hccol(lim), hcnnz, flag
    real(kind=8), intent(out) :: hcval(lim)
    hcnnz = 0
    lmem = .false.
    flag = 0
end subroutine myevalhc

subroutine myevalfc(n,x,f,m,c,flag)
    integer, intent(in) :: n, m
    real(kind=8), intent(in) :: x(n)
    real(kind=8), intent(out) :: f, c(m)
    integer, intent(out) :: flag
    call myevalf(n,x,f,flag)
end subroutine myevalfc

subroutine myevalgjac(n,x,g,m,jcfun,jcvar,jcval,jcnnz,lim,lmem,flag)
    integer, intent(in) :: n, m, lim
    real(kind=8), intent(in) :: x(n)
    logical, intent(out) :: lmem
    integer, intent(out) :: jcfun(lim), jcvar(lim), jcnnz, flag
    real(kind=8), intent(out) :: g(n), jcval(lim)
    call myevalg(n,x,g,flag)
    jcnnz = 0
    lmem = .false.
end subroutine myevalgjac

subroutine myevalgjacp(n,x,g,m,p,q,work,gotj,flag)
    integer, intent(in) :: n, m, p, q
    real(kind=8), intent(in) :: x(n)
    logical, intent(in) :: gotj
    real(kind=8), intent(out) :: g(n), work(q)
    integer, intent(out) :: flag
    call myevalg(n,x,g,flag)
end subroutine myevalgjacp

subroutine myevalhl(n,x,m,lambda,sf,sc,hllin,hlcol,hlval,hlnnz,lim,lmem,flag)
    integer, intent(in) :: n, m, lim
    real(kind=8), intent(in) :: x(n), lambda(m), sf, sc(m)
    logical, intent(out) :: lmem
    integer, intent(out) :: hllin(lim), hlcol(lim), hlnnz, flag
    real(kind=8), intent(out) :: hlval(lim)
    flag = -1
    lmem = .false.
    hlnnz = 0
end subroutine myevalhl

subroutine myevalhlp(n,x,m,lambda,sf,sc,p,hp,goth,flag)
    implicit none
    logical, intent(inout) :: goth
    integer, intent(in) :: n, m
    integer, intent(out) :: flag
    real(kind=8), intent(in) :: sf
    real(kind=8), intent(in) :: lambda(m), p(n), sc(m), x(n)
    real(kind=8), intent(out) :: hp(n)
    flag = -1
end subroutine myevalhlp


! ----------------------------------------------------------------------
! SUBROUTINE TO SET CURRENT PROBLEM NUMBER
! ----------------------------------------------------------------------
subroutine set_problem(nprob)
    use problem_control
    implicit none
    integer, intent(in) :: nprob
end subroutine set_problem
