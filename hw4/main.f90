program main
    use types
    use dataanalyses
    implicit none

    ! Premenné
    integer(kind=ikind) :: i, n, io_status
    real(kind=rkind), dimension(:), allocatable :: tp_cum, e_cum ! Vstupy
    real(kind=rkind), dimension(:), allocatable :: tp_inc, e_inc ! Výstupy
    character(len=100) :: dummy_string

    ! 1. Zistenie počtu riadkov v súbore
    open(unit=10, file='era5_input.txt', status='old', action='read')
    n = 0
    do
        read(10, *, iostat=io_status) dummy_string ! Skúšobné čítanie
        if (io_status /= 0) exit ! Koniec súboru
        n = n + 1
    end do
    close(10)

    print *, "Pocet riadkov v subore: ", n

    ! 2. Alokácia pamäte
    allocate(tp_cum(n))
    allocate(e_cum(n))
    allocate(tp_inc(n))
    allocate(e_inc(n))

    ! 3. Načítanie dát
    open(unit=10, file='era5_input.txt', status='old', action='read')
    do i = 1, n

        read(10, *) dummy_string, tp_cum(i), e_cum(i)
    end do
    close(10)

    ! 4. V dataanalyses
    call correct_era5_daily_cum(tp_cum, e_cum, tp_inc, e_inc)

    ! 5. Výpis na obrazovku
    print *, "--- Kontrola prvych 10 zaznamov ---"
    print *, "Idx | TP_Orig | TP_New | E_Orig  | E_New"
    do i = 1, 10
        if (i > n) exit
        print '(I4, 4F10.4)', i, tp_cum(i), tp_inc(i), e_cum(i), e_inc(i)
    end do

    ! 6. Zápis do súboru
    open(unit=20, file='era5_corrected.txt', status='replace', action='write')
    do i = 1, n
        write(20, '(2F12.6)') tp_inc(i), e_inc(i)
    end do
    close(20)

    print *, "Hotovo! Vysledok je v era5_corrected.txt"

    ! Upratanie pamäte
    deallocate(tp_cum, e_cum, tp_inc, e_inc)

end program main
