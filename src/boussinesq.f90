program boussinesq
  use iso_fortran_env
  use utils
  implicit none
  integer(int32), parameter :: N_lambda = 64, N_theta = 32
  integer(int32) :: num_steps, i
  real(wp), dimension(N_lambda,N_theta) :: u, u_prev, v, v_prev, phi, &
       phi_prev, uc
  real(wp) :: d_theta, d_lambda, dt, initial_energy, final_energy
  d_theta = pi/N_theta
  d_lambda = 2*pi/N_lambda
  dt = (d_lambda)**2
  num_steps = 1000

  call set_gaussian(u, 0.125_wp)
  call set_gaussian(v, 0.125_wp)
  call set_gaussian(phi, 0.25_wp)
  initial_energy = energy(u, v, phi, d_theta)

  do i=1, num_steps
     u_prev = u
     v_prev = v
     phi_prev = phi
     uc = mul_sec_theta(u_prev, d_theta)
     u = u_prev &
          - dt*inv_earth_radius*(mult_fields(uc, diff_lambda(u_prev)/d_lambda) &
          + mult_fields(v_prev, diff_theta(u_prev)/d_theta) &
          + mul_sec_theta(diff_lambda(phi_prev)/d_lambda, d_theta))
     v = v_prev &
          - dt*inv_earth_radius*(mult_fields(uc, diff_lambda(v_prev)/d_lambda) &
          + mult_fields(v_prev, diff_theta(v_prev)/d_theta) &
          + mul_sec_theta(diff_theta(phi_prev)/d_theta, d_theta))
     phi = phi_prev &
          - dt*inv_earth_radius*(mult_fields(uc, diff_lambda(phi_prev)/d_lambda) &
          + mult_fields(v_prev, diff_theta(phi_prev)/d_theta))
  end do
  final_energy = energy(u, v, phi, d_theta)
  print *, "initial energy = ", initial_energy
  print *, "final energy = ", final_energy
  print *, "difference = ", final_energy - initial_energy
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
    theta = ((n - 0.5_wp)*d_theta) - (0.5_wp)*pi
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
    real(wp) :: avg, diff_theta(size(field, dim=1), size(field, dim=2))
    integer(int32) :: M, N
    M = size(field, dim=2)
    N = size(field, dim=1)
    ! centered difference for periodic function
    diff_theta(:, 2:M-1) = 0.5*(field(:, 3:M)-field(:, 1:M-2))
    ! boundary conditions
    ! north pole
    avg = SUM(field(:,M))/N
    diff_theta(:,M) = 0.5*(avg - field(:,M-1))
    ! south pole
    avg = SUM(field(:,1))/N
    diff_theta(:,1) = 0.5*(field(:,2)-avg)
  end function diff_theta

  pure function energy_density(u, v, phi, i_lambda, j_theta, d_theta)
    real(wp), dimension(:,:), intent(in) :: u, v, phi
    integer(int32), intent(in) :: i_lambda, j_theta
    real(wp), intent(in) :: d_theta
    real(wp) :: energy_density
    energy_density = (0.5*(u(i_lambda, j_theta)**2 + v(i_lambda, j_theta)**2) &
         + phi(i_lambda, j_theta))*cos(theta(j_theta, d_theta))
  end function energy_density

  pure function energy(u, v, phi, d_theta)
    real(wp), dimension(:,:), intent(in) :: u, v, phi
    real(wp), intent(in) :: d_theta
    real(wp) :: energy
    integer(int32) :: i, j
    energy = 0.0_wp
    do i=1, N_lambda
       energy = energy + 0.5*(energy_density(u, v, phi, i, 1, d_theta) &
            + energy_density(u, v, phi, i, N_theta, d_theta))
       do j=2, N_theta-1
          energy = energy + energy_density(u, v, phi, i, j, d_theta)
       end do
    end do
    energy = energy * d_theta * (2*pi/N_lambda)
  end function energy
end program boussinesq
