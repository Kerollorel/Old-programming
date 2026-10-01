module types

    implicit none

    ! 32-bit integer (about 9-10 decimal digits)
    integer, parameter :: ikind = selected_int_kind(9)
    ! double precision real (~15 digits, wide exponent range)
    integer, parameter :: rkind = selected_real_kind(p=15, r=307)

end module types
