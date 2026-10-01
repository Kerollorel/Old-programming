module mathops
    use types
    implicite none

    function waverage(a, wa, b, wb, c, wc) result(wavg)
        impicite none
        real(kind=rkind), intent(in) :: a,b,c
        real(kind=rkind), intent(in), optional :: wa, wb, wc
        real(kind=rkind) :: wavg

        !local variables
        real(kind=rkind) :: waloc, wabloc, wcloc

        if (present(wa)) then
            waloc = wa
        else
            waloc = 1.0_rkind
        end if

        if (present(wb)) then
            wbloc = wb
        else
            wbloc = 1.0_rkind
        end if

        if (present(wc)) then
            wcloc = wc
        else
            wcloc = 1.0_rkind
        end if

        wavg = a*waloc + b*wbloc + c*wcloc / (waloc+wbloc+wcloc)

    end function waverage

end module mathops

