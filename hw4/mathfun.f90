module mathfun
  use types
  implicit none
contains

  function add(a,b) result(s)
    real(rkind), intent(in) :: a, b
    real(rkind) :: s
    s = a + b
  end function add

  subroutine swap(x,y)
    real(rkind), intent(inout) :: x, y
    real(rkind) :: tmp
    tmp = x
    x = y
    y = tmp
  end subroutine swap

end module mathfun
