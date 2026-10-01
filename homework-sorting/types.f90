module types
  implicit none
  ! explicit kinds required by the assignment
  integer, parameter :: rkind = selected_real_kind(15, 307)
  integer, parameter :: ikind = selected_int_kind(9)
end module types
