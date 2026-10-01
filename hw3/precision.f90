program signifi

    implicit none

    real(kind=8) :: big, small, sum
    integer :: n

    big = 1
    small = 1
    n = 0

    print *, "          n", "    small", "                    big", "                       sum"
    print *, "--------------------------------------------------------------------------------------"

    do
        sum = big + small
        print *, n, small, big, sum

        if (sum <= big) then
            print *, "At step",n, "adding SMALL no longer increases BIG."
            print *, "Machine epsilon is approximately: ", small
            exit
        end if

        small = small / 2
        n = n + 1
    end do
end program signifi
