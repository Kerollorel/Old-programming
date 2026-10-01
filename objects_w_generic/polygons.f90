module polygons_mod
  use kinds, only: rkind
  use shapes_mod
  implicit none

  type, extends(shape) :: right_triangle
    real(kind=rkind) :: a = 0.0_rkind
    real(kind=rkind) :: b = 0.0_rkind
  contains
    procedure :: area      => right_triangle_area
    procedure :: perimeter => right_triangle_perimeter
  end type right_triangle

  type, extends(shape) :: triangle_xy
    real(kind=rkind) :: x1=0.0_rkind, y1=0.0_rkind
    real(kind=rkind) :: x2=0.0_rkind, y2=0.0_rkind
    real(kind=rkind) :: x3=0.0_rkind, y3=0.0_rkind
  contains
    procedure :: area      => triangle_xy_area
    procedure :: perimeter => triangle_xy_perimeter
  end type triangle_xy

  type, extends(shape) :: quadrangle_xy
    real(kind=rkind) :: x1=0.0_rkind, y1=0.0_rkind
    real(kind=rkind) :: x2=0.0_rkind, y2=0.0_rkind
    real(kind=rkind) :: x3=0.0_rkind, y3=0.0_rkind
    real(kind=rkind) :: x4=0.0_rkind, y4=0.0_rkind
  contains
    procedure :: area      => quadrangle_xy_area
    procedure :: perimeter => quadrangle_xy_perimeter
  end type quadrangle_xy

contains

  function right_triangle_area(self) result(ar)
    class(right_triangle), intent(in) :: self
    real(kind=rkind) :: ar
    ar = 0.5_rkind * self%a * self%b
  end function right_triangle_area

  function right_triangle_perimeter(self) result(p)
    class(right_triangle), intent(in) :: self
    real(kind=rkind) :: p
    p = self%a + self%b + sqrt(self%a*self%a + self%b*self%b)
  end function right_triangle_perimeter

  function triangle_xy_area(self) result(ar)
    class(triangle_xy), intent(in) :: self
    real(kind=rkind) :: ar
    ar = 0.5_rkind * abs( &
       self%x1*(self%y2 - self%y3) + &
       self%x2*(self%y3 - self%y1) + &
       self%x3*(self%y1 - self%y2) )
  end function triangle_xy_area

  function triangle_xy_perimeter(self) result(p)
    class(triangle_xy), intent(in) :: self
    real(kind=rkind) :: p
    p = distance2(self%x1,self%y1,self%x2,self%y2) + &
        distance2(self%x2,self%y2,self%x3,self%y3) + &
        distance2(self%x3,self%y3,self%x1,self%y1)
  end function triangle_xy_perimeter

  function quadrangle_xy_area(self) result(ar)
    class(quadrangle_xy), intent(in) :: self
    real(kind=rkind) :: ar
    ar = triangle_area_coords(self%x1,self%y1,self%x2,self%y2,self%x3,self%y3) + &
         triangle_area_coords(self%x1,self%y1,self%x3,self%y3,self%x4,self%y4)
  end function quadrangle_xy_area

  function quadrangle_xy_perimeter(self) result(p)
    class(quadrangle_xy), intent(in) :: self
    real(kind=rkind) :: p
    p = distance2(self%x1,self%y1,self%x2,self%y2) + &
        distance2(self%x2,self%y2,self%x3,self%y3) + &
        distance2(self%x3,self%y3,self%x4,self%y4) + &
        distance2(self%x4,self%y4,self%x1,self%y1)
  end function quadrangle_xy_perimeter

  pure function distance2(x1,y1,x2,y2) result(d)
    real(kind=rkind), intent(in) :: x1,y1,x2,y2
    real(kind=rkind) :: d
    d = sqrt( (x2-x1)**2 + (y2-y1)**2 )
  end function distance2

  pure function triangle_area_coords(x1,y1,x2,y2,x3,y3) result(ar)
    real(kind=rkind), intent(in):: x1,y1,x2,y2,x3,y3
    real(kind=rkind) :: ar
    ar = 0.5_rkind * abs( x1*(y2-y3) + x2*(y3-y1) + x3*(y1-y2) )
  end function triangle_area_coords

end module polygons_mod
