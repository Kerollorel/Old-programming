program overflow

  implicit none

  integer(kind=4) :: fact4, prev_fact4
  integer(kind=8) :: fact8, prev_fact8
  integer :: n


  fact4 = 1
  prev_fact4 = 1
  n = 1
  do
    prev_fact4 = fact4
    fact4 = fact4 * n
    if (fact4 < prev_fact4) then
      print *, "Overflow detected for INTEGER(4) at n =", n
      print *, "Last correct factorial =", prev_fact4
      exit
    end if
    n = n + 1
  end do


  fact8 = 1
  prev_fact8 = 1
  n = 1
  do
    prev_fact8 = fact8
    fact8 = fact8 * n
    if (fact8 < prev_fact8) then
      print *, "Overflow detected for INTEGER(8) at n =", n
      print *, "Last correct factorial =", prev_fact8
      exit
    end if
    n = n + 1
  end do

end program overflow
