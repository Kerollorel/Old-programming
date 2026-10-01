program homework_main
  use kinds, only: rkind
  use vectors_mod
  use shapes_mod
  use polygons_mod
  use solids_mod
  use generic_area_mod
  implicit none

  ! Declarations
  type(vector2d) :: v1, v2, v3
  type(vector3d) :: vv3

  class(shape), allocatable :: s1, s2, s3
  type(rectangle) :: rec
  type(right_triangle) :: rt
  type(triangle_xy) :: tri
  type(quadrangle_xy) :: quad

  type(tetrahedron_xyz) :: tet
  class(solid), allocatable :: solid1


  ! Code

  ! Task 1: vector sum test
  v1%x = 1.0_rkind; v1%y = 2.0_rkind
  v2%x = 3.0_rkind; v2%y = 4.0_rkind
  v3 = v1%sum(v2)
  write(*,'(A,2F8.3)') "Vector sum result (expect 4.0 6.0): ", v3%x, v3%y
  write(*,'(A,F8.3)')    "v1 norm (expect sqrt(5)) = ", v1%norm()

  ! vector3d test
  vv3%x = 1.0_rkind; vv3%y = 2.0_rkind; vv3%z = 2.0_rkind
  write(*,'(A,F8.3)') "vector3d norm (expect 3.0) = ", vv3%norm()

  ! Prepare shapes
  rec%width = 2.0_rkind; rec%height = 5.0_rkind
  rt%a = 3.0_rkind; rt%b = 4.0_rkind
  tri%x1 = 0.0_rkind; tri%y1 = 0.0_rkind
  tri%x2 = 1.0_rkind; tri%y2 = 0.0_rkind
  tri%x3 = 0.0_rkind; tri%y3 = 1.0_rkind
  quad%x1 = 0.0_rkind; quad%y1 = 0.0_rkind
  quad%x2 = 2.0_rkind; quad%y2 = 0.0_rkind
  quad%x3 = 2.0_rkind; quad%y3 = 5.0_rkind
  quad%x4 = 0.0_rkind; quad%y4 = 5.0_rkind

  ! Compiler decides
  write(*,'(A)') "=== Generic (compile-time) area(...) calls ==="
  write(*,'(A,F8.3)') "area(rectangle) = ", area(rec)
  write(*,'(A,F8.3)') "area(right_triangle) = ", area(rt)
  write(*,'(A,F8.3)') "area(triangle_xy) = ", area(tri)
  write(*,'(A,F8.3)') "area(quadrangle_xy) = ", area(quad)


  allocate(s1, source=rec)
  allocate(s2, source=rt)
  allocate(s3, source=tri)

  write(*,'(A)') "=== Polymorphic (runtime) s%area() calls ==="
  call print_shape(s1)
  call print_shape(s2)
  call print_shape(s3)

  write(*,'(A,F8.3)') "Quadrangle (rectangle corners) area (expect 10) = ", quad%area()
  write(*,'(A,F8.3)') "Quadrangle perimeter (expect 14) = ", quad%perimeter()


  tet%x1 = 0.0_rkind; tet%y1 = 0.0_rkind; tet%z1 = 0.0_rkind
  tet%x2 = 1.0_rkind; tet%y2 = 0.0_rkind; tet%z2 = 0.0_rkind
  tet%x3 = 0.0_rkind; tet%y3 = 1.0_rkind; tet%z3 = 0.0_rkind
  tet%x4 = 0.0_rkind; tet%y4 = 0.0_rkind; tet%z4 = 3.0_rkind
  write(*,'(A,F8.3)') "Tetrahedron volume (expect 0.5) = ", tet%volume()


  allocate(solid1, source=tet)
  write(*,'(A,F8.3)') "Polymorphic solids(1) volume = ", solid1%volume()

contains

 subroutine print_shape(s)
    class(shape), intent(in) :: s
    select type(ss => s)
    class is (rectangle)
      write(*,'(A,F8.3,A,F8.3)') "Rectangle: area=", ss%area(), " perimeter=", ss%perimeter()
    class is (right_triangle)
      write(*,'(A,F8.3,A,F8.3)') "Right triangle: area=", ss%area(), " perimeter=", ss%perimeter()
    class is (triangle_xy)
      write(*,'(A,F8.3,A,F8.3)') "Triangle_xy: area=", ss%area(), " perimeter=", ss%perimeter()
    class default
      write(*,'(A)') "Unknown shape"
    end select
  end subroutine print_shape

end program homework_main
