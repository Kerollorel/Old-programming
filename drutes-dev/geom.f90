module geom
    use types
    implicit none
    private
    public :: carea, rectgeom, tarea, cylvol

contains

    !circle area: pi * r^2
    function carea(r) result(a)
        implicit none
        real(kind=rkind), intent(in) :: r
        real(kind=rkind) :: a
        real(kind=rkind) :: pi
        pi = 4.0_rkind * atan(1.0_rkind)
        a = pi * r**2
    end function carea

     !rectanglearea&perimeter
     subroutine rectgeom(x,y,ar, pe)
        implicit none
        real(kind=rkind), intent(in) :: x,y
        real(kind=rkind), intent(out):: ar,pe
        ar=x*y
        pe=2.0_rkind*(x+y)
    end subroutine rectgeom

    !trianglearea:0.5*b*h
    function tarea(b,h) result(a)
        implicit none
        real(kind=rkind), intent(in) :: b,h
        real(kind=rkind) :: a
        a = 0.5_rkind*b*h
    end function tarea

    !cylindervolume:pi*r^2*h
    function cylvol(r,h)result(v)
        implicit none
        real(kind=rkind), intent(in) ::r,h
        real(kind=rkind) :: v
        real(kind=rkind)::pi
        pi=4.0_rkind*atan(1.0_rkind)
        v=pi*r**2*h
    end function cylvol
end module geom
