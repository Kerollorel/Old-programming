module dataanalyses
    use types
    implicit none

contains

    ! 1. Subroutine na jednoduchý rozdiel (teraz - včera)
    subroutine diff1(x, dx)
        real(kind=rkind), dimension(:), intent(in)  :: x  ! Vstup (kumulatívne)
        real(kind=rkind), dimension(:), intent(out) :: dx ! Výstup (rozdiely)
        integer(kind=ikind) :: i, n

        n = size(x)
        ! Prvý prvok nemá "predchodcu"
        if (n > 0) dx(1) = x(1)


        do i = 2, n
            dx(i) = x(i) - x(i-1)
        end do
    end subroutine diff1

    ! 2. Hlavná logika na opravu ERA5 dát
    subroutine correct_era5_daily_cum(tp_cum, e_cum, tp_inc, e_inc)
        real(kind=rkind), dimension(:), intent(in)  :: tp_cum, e_cum ! Vstupy
        real(kind=rkind), dimension(:), intent(out) :: tp_inc, e_inc ! Výstupy

        ! Pomocné polia na rozdiely
        real(kind=rkind), dimension(:), allocatable :: tp_diff, e_diff
        integer(kind=ikind) :: n, i, hrs

        n = size(tp_cum)
        allocate(tp_diff(n), e_diff(n))


        call diff1(tp_cum, tp_diff)
        call diff1(e_cum, e_diff)

        hrs = 0 ! Počítadlo hodín

        do i = 1, n
            hrs = hrs + 1

            if (hrs == 1) then
                ! Je prvá hodina dňa (po resete).
                tp_inc(i) = tp_cum(i)
                e_inc(i)  = e_cum(i)
            else
                ! Je bežná hodina. Použijeme rozdiel.
                tp_inc(i) = tp_diff(i)
                e_inc(i)  = e_diff(i)
            end if

            ! Reset počítadla po 24 hodinách
            if (hrs == 24) hrs = 0

            ! Orezanie nezmyselných hodnôt
            if (tp_inc(i) < 0.0_rkind) tp_inc(i) = 0.0_rkind

            ! Evaporácia ak je v ERA5 záporná, daj 0
            if (e_inc(i) > 0.0_rkind) e_inc(i) = 0.0_rkind
        end do

        deallocate(tp_diff, e_diff)
    end subroutine correct_era5_daily_cum

end module dataanalyses
