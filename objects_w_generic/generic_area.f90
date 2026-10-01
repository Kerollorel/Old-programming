module generic_area_mod
  use shapes_mod       ! provides rectangle_area
  use polygons_mod     ! provides right_triangle_area, triangle_xy_area, quadrangle_xy_area
  implicit none
  private
  public :: area

  ! Generic interface appears BEFORE any CONTAINS block
  interface area
    module procedure right_triangle_area
    module procedure rectangle_area
    module procedure triangle_xy_area
    module procedure quadrangle_xy_area
  end interface area

end module generic_area_mod
