program boussinesq
  use iso_fortran_env
  use utils
  implicit none
  integer(int32) :: N, num_steps, i
  real(wp), dimension(32,32) :: u, u_prev, v, v_prev, phi, phi_prev, uc, &
       u_lambda, u_theta, v_lambda, v_theta, phi_lambda, phi_theta
  real(wp) :: d_theta, d_lambda, dt
  N = 32
  d_theta = pi/33.0_wp
  d_lambda = 2*pi/32.0_wp
  dt = sqrt(d_lambda) !0.001_wp
  num_steps = 1000

  call set_gaussian(u, 0.125_wp)
  call set_gaussian(v, 0.125_wp)
  call set_gaussian(phi, 0.25_wp)

  do i=1, num_steps
     u_prev = u
     v_prev = v
     phi_prev = phi
     uc = mul_sec_theta(u_prev, d_theta)
     u = u_prev &
          - dt*inv_earth_radius*(mult_fields(uc, diff_lambda(u_prev))/d_lambda &
          + mult_fields(v_prev, diff_theta(u_prev))/d_theta &
          + mul_sec_theta(diff_lambda(phi_prev), d_theta)/d_lambda)
     v = v_prev &
          - dt*inv_earth_radius*(mult_fields(uc, diff_lambda(v_prev)/d_lambda) &
          + mult_fields(v_prev, diff_theta(v_prev)) &
          + mul_sec_theta(diff_theta(phi_prev)/d_theta, d_theta))
     phi = phi_prev &
          - dt*inv_earth_radius*(mult_fields(uc, diff_lambda(phi_prev)/d_lambda) &
          + mult_fields(v_prev, diff_theta(phi_prev)/d_theta))
  end do
  print *, "u = ", u
contains

  pure function mult_fields(lhs, rhs) result(ans)
    real(wp), dimension(:,:), intent(in) :: lhs, rhs
    real(wp) :: ans(size(lhs,dim=1),size(lhs,dim=2))
    integer(int32) :: i, j, max_lambda, max_theta
    max_lambda = size(lhs,dim=1)
    max_theta = size(lhs,dim=2)
    do concurrent(i = 1:max_lambda, j=1:max_theta)
       ans(i,j) = lhs(i,j)*rhs(i,j)
    end do
  end function mult_fields
    
  subroutine set_gaussian(field, decay)
    real(wp), dimension(:,:), intent(in out) :: field
    real(wp), intent(in) :: decay
    integer(int32) :: i, j, max_lambda, max_theta
    max_lambda = size(field,dim=1)
    max_theta = size(field,dim=2)
    do concurrent(i = 1:max_lambda, j=1:max_theta)
       field(i,j) = exp(-decay*((i - max_lambda/2)**2 + (j - max_theta/2)**2))
    end do
  end subroutine set_gaussian

  pure function theta(n, d_theta)
    integer(int32), intent(in) :: n
    real(wp), intent(in) :: d_theta
    real(wp) :: theta
    ! assert(1 <= n <= N)
    theta = (n*d_theta) - (0.5_wp)*pi
  end function theta

  pure function mul_sec_theta(field, d_theta) result(ans)
    real(wp), dimension(:,:), intent(in) :: field
    real(wp) :: ans(size(field, dim=1), size(field, dim=2))
    real(wp), intent(in) :: d_theta
    integer(int32) :: i

    do i=1,size(field,dim=2)
       ans(:,i) = field(:,i)/cos(theta(i,d_theta))
    end do
  end function mul_sec_theta

  pure function diff_lambda(field)
    real(wp), dimension(:,:), intent(in) :: field
    real(wp) :: diff_lambda(size(field, dim=1), size(field, dim=2))
    integer(int32) :: M
    M = size(field, dim=1)
    ! centered difference for periodic function
    diff_lambda(2:M-1, :) = 0.5*(field(3:M, :)-field(1:M-2, :))
    diff_lambda(M, :) = 0.5*(field(1, :) - field(M-1, :))
    diff_lambda(1, :) = 0.5*(field(2, :) - field(M, :))
  end function diff_lambda

  pure function diff_theta(field)
    real(wp), dimension(:,:), intent(in) :: field
    real(wp) :: diff_theta(size(field, dim=1), size(field, dim=2))
    integer(int32) :: M, i
    M = size(field, dim=2)
    ! centered difference for periodic function
    diff_theta(:, 2:M-1) = 0.5*(field(:, 3:M)-field(:, 1:M-2))
    ! boundary conditions
    do i=1,(M/2)
       ! bottom
       diff_theta(i,1) = 0.5*(field(i,2)-field(M-i+1,1))
       diff_theta(M-i+1,1) = 0.5*(field(M-i+1,2)-field(i,1))
       ! top
       diff_theta(i,M) = 0.5*(field(M-i+1,M)-field(i,M-1))
       diff_theta(i-i+1,M) = 0.5*(field(i,M)-field(i,M-1))
    end do
  end function diff_theta
  
end program boussinesq