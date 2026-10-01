program main
    use types
    use dataanalyses
    implicit none

    ! Premenné
    integer(kind=ikind) :: i, n, io_status, fileid,  ierr
    real(kind=rkind), dimension(:), allocatable :: tp_cum, e_cum ! Vstupy
    real(kind=rkind), dimension(:), allocatable :: tp_inc, e_inc ! Výstupy
    character(len=100) :: dummy_string
    real(kind=rkind) :: temp_tp, temp_e

    ! 1. Zistenie počtu riadkov v súbore
    open(newunit=fileid, file='era5_input.txt', status='old', action='read', iostat=ierr)
    ! Kontrola, či sa súbor podarilo otvoriť
    if (ierr /= 0) then
        print *, "Chyba: Subor era5_input.txt sa nepodarilo otvorit!"
        stop
    end if

    n = 0
    do
        ! OPRAVENÝ RIADOK
        read(fileid, *, iostat=io_status) dummy_string
        if (io_status /= 0) exit
        n = n + 1
    end do
    close(fileid)

    print *, "Pocet riadkov v subore: ", n

    ! 2. Alokácia pamäte
    allocate(tp_cum(n))
    allocate(e_cum(n))
    allocate(tp_inc(n))
    allocate(e_inc(n))

    ! 3. Načítanie dát
    open(newunit=fileid, file='era5_input.txt', status='old', action='read')
    do i = 1, n

        read(fileid, *, iostat=io_status) dummy_string, temp_tp, temp_e

        if (io_status == 0) then
            ! --- VŠETKO JE OK ---
            tp_cum(i) = temp_tp
            e_cum(i)  = temp_e
        else
            print *, "POZOR: Chyba formatu na riadku cislo:", i
            print *, "       Opravujem: pouzivam hodnotu z predchadzajucej hodiny."

            if (i > 1) then
                ! Skopírujeme hodnotu z predchádzajúceho riadku.
                tp_cum(i) = tp_cum(i-1)
                e_cum(i)  = e_cum(i-1)
            else
                ! Ak by bola chyba hneď na prvom riadku, dáme nuly.
                tp_cum(i) = 0.0_rkind
                e_cum(i)  = 0.0_rkind
            end if
        end if
    end do
    close(fileid)

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
    open(newunit=fileid, file='era5_corrected.txt', status='replace', action='write')
    do i = 1, n
        write(fileid, '(2F14.6)') tp_inc(i), e_inc(i)
    end do
    close(fileid)

    print *, "Hotovo! Vysledok je v era5_corrected.txt"

    ! Upratanie pamäte
    deallocate(tp_cum, e_cum, tp_inc, e_inc)

end program main
