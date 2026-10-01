module geom2d
  use types
  implicit none
contains

  function carea(r) result(A)
    real(rkind), intent(in) :: r
    real(rkind) :: A
    real(rkind) :: pi
    pi = 4.0_rkind * atan(1.0_rkind)
    A = pi * r * r
  end function carea

  function sarea(a) result(res)
    real(rkind), intent(in) :: a
    real(rkind) :: res
    res = a * a
  end function sarea

  subroutine rectap(a,b,area,P)
    real(rkind), intent(in)  :: a, b
    real(rkind), intent(out) :: area, P
    area = a * b
    P = 2.0_rkind * (a + b)
  end subroutine rectap

end module geom2d
