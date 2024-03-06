module utils
  use iso_fortran_env, only: real64, int32
  implicit none
      integer, parameter :: wp = real64
    real(wp), parameter :: pi = 3.14159265358979323846264338327950288_wp
    real(wp), parameter :: earth_radius = 6366.1977236758134307553505349005744813783858_wp
    real(wp), parameter :: inv_earth_radius = 0.000157079632679489661923132169163975144_wp
  contains
    subroutine assert_failed(file, line_number, test, msg)
      implicit none
      character(*), intent(in) :: file, test, msg
      integer, intent(in) :: line_number
      print *, "Assert: ",trim(test)," Failed at ",trim(file),":",line_number
      print *, "Msg:", trim(msg)
      stop
    end subroutine assert_failed
end module utils
