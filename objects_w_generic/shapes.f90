module shapes_mod
  use kinds, only: rkind
  implicit none

  type, abstract :: shape
  contains
    procedure(area_ifc), deferred :: area
    procedure(perimeter_ifc), deferred :: perimeter
  end type shape

  abstract interface
    function area_ifc(self) result(a)
      import :: shape, rkind
      class(shape), intent(in) :: self
      real(kind=rkind) :: a
    end function area_ifc

    function perimeter_ifc(self) result(p)
      import :: shape, rkind
      class(shape), intent(in) :: self
      real(kind=rkind) :: p
    end function perimeter_ifc
  end interface

  type, extends(shape) :: rectangle
    real(kind=rkind) :: width = 0.0_rkind
    real(kind=rkind) :: height = 0.0_rkind
  contains
    procedure :: area      => rectangle_area
    procedure :: perimeter => rectangle_perimeter
  end type rectangle

contains

  function rectangle_area(self) result(a)
    class(rectangle), intent(in) :: self
    real(kind=rkind) :: a
    a = self%width * self%height
  end function rectangle_area

  function rectangle_perimeter(self) result(p)
    class(rectangle), intent(in) :: self
    real(kind=rkind) :: p
    p = 2.0_rkind * (self%width + self%height)
  end function rectangle_perimeter

end module shapes_mod
