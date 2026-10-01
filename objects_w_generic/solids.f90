module solids_mod
  use kinds, only: rkind
  implicit none

  type, abstract :: solid
  contains
    procedure(volume_ifc), deferred :: volume
  end type solid

  abstract interface
    function volume_ifc(self) result(v)
      import :: solid, rkind
      class(solid), intent(in) :: self
      real(kind=rkind) :: v
    end function volume_ifc
  end interface

  type, extends(solid) :: tetrahedron_xyz
    real(kind=rkind) :: x1=0.0_rkind, y1=0.0_rkind, z1=0.0_rkind
    real(kind=rkind) :: x2=0.0_rkind, y2=0.0_rkind, z2=0.0_rkind
    real(kind=rkind) :: x3=0.0_rkind, y3=0.0_rkind, z3=0.0_rkind
    real(kind=rkind) :: x4=0.0_rkind, y4=0.0_rkind, z4=0.0_rkind
  contains
    procedure :: volume => tetrahedron_volume
  end type tetrahedron_xyz

contains

  function tetrahedron_volume(self) result(vol)
    class(tetrahedron_xyz), intent(in) :: self
    real(kind=rkind) :: vol
    real(kind=rkind) :: a(3), b(3), c(3), cp(3)
    a = (/ self%x2 - self%x1, self%y2 - self%y1, self%z2 - self%z1 /)
    b = (/ self%x3 - self%x1, self%y3 - self%y1, self%z3 - self%z1 /)
    c = (/ self%x4 - self%x1, self%y4 - self%y1, self%z4 - self%z1 /)
    cp = cross3(b, c)
    vol = abs(dot3(a, cp)) / 6.0_rkind
  end function tetrahedron_volume

  pure function cross3(u, v) result(w)
    real(kind=rkind), intent(in) :: u(3), v(3)
    real(kind=rkind) :: w(3)
    w(1) = u(2)*v(3) - u(3)*v(2)
    w(2) = u(3)*v(1) - u(1)*v(3)
    w(3) = u(1)*v(2) - u(2)*v(1)
  end function cross3

  pure function dot3(u, v) result(d)
    real(kind=rkind), intent(in) :: u(3), v(3)
    real(kind=rkind) :: d
    d = u(1)*v(1) + u(2)*v(2) + u(3)*v(3)
  end function dot3

end module solids_mod
