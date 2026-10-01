program demo_geom
    use types
    use geom

    implicit none

    real(kind=rkind)::r,x,y,ar,pe
    r=2.0_rkind
    print *,"Circle area(r=2):",carea(r)
    x=3.0_rkind; y=4.0_rkind
    call rectgeom(x,y,ar,pe)
    print *,"Rect area, perim:", ar,pe

    print *,"Tri area(3x4):",tarea(3.0_rkind,4.0_rkind)
    print *,"Cyl vol(r=1.5,h=10):", cylvol(1.5_rkind,10.0_rkind)
end program demo_geom
