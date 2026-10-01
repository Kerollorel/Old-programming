module vectors_mod
  use kinds, only: rkind
  implicit none

  type :: vector2d
    real(kind=rkind) :: x = 0.0_rkind
    real(kind=rkind) :: y = 0.0_rkind
  contains
    procedure :: norm => vector2d_norm
    procedure :: sum  => vector2d_sum
  end type vector2d

  type, extends(vector2d) :: vector3d
    real(kind=rkind) :: z = 0.0_rkind
  contains
    procedure :: norm => vector3d_norm
  end type vector3d

contains

  function vector2d_norm(self) result(val)
    class(vector2d), intent(in) :: self
    real(kind=rkind) :: val
    val = sqrt(self%x*self%x + self%y*self%y)
  end function vector2d_norm

  function vector2d_sum(self, other) result(res)
    class(vector2d), intent(in) :: self, other
    type(vector2d) :: res
    res%x = self%x + other%x
    res%y = self%y + other%y
  end function vector2d_sum

  function vector3d_norm(self) result(val)
    class(vector3d), intent(in) :: self
    real(kind=rkind) :: val
    val = sqrt(self%x*self%x + self%y*self%y + self%z*self%z)
  end function vector3d_norm

end module vectors_mod
