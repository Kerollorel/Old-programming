module kinds
  implicit none
  integer, parameter :: rkind = selected_real_kind(15, 307)
  integer, parameter :: ikind = selected_int_kind(9)
end module kinds
