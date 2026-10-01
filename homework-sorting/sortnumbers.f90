module sortnumbers
  use types
  implicit none

contains

  ! Non-destructive descending sort (selection using maxloc on sections)
  subroutine sorthighlow(array_in, array_out)
    real(kind=rkind), intent(in),  dimension(:)             :: array_in
    real(kind=rkind), intent(out), allocatable, dimension(:) :: array_out
    integer(kind=ikind) :: n, i
    integer(kind=ikind), dimension(1) :: pos
    real(kind=rkind) :: tmp

    n = size(array_in)
    allocate(array_out(n))
    if (n <= 1) then
      if (n == 1) array_out = array_in
      return
    end if

    ! whole-array copy as required
    array_out = array_in

    do i = 1, n
      pos = maxloc(array_out(i:))      ! 1-based within the slice
      pos(1) = pos(1) + i - 1          ! rebase to absolute index
      if (pos(1) /= i) then
        tmp = array_out(i)
        array_out(i) = array_out(pos(1))
        array_out(pos(1)) = tmp
      end if
    end do
  end subroutine sorthighlow


  ! Non-destructive ascending sort (selection using minloc on sections)
  subroutine sortlowhigh(array_in, array_out)
    real(kind=rkind), intent(in),  dimension(:)             :: array_in
    real(kind=rkind), intent(out), allocatable, dimension(:) :: array_out
    integer(kind=ikind) :: n, i
    integer(kind=ikind), dimension(1) :: pos
    real(kind=rkind) :: tmp

    n = size(array_in)
    allocate(array_out(n))
    if (n <= 1) then
      if (n == 1) array_out = array_in
      return
    end if

    array_out = array_in

    do i = 1, n
      pos = minloc(array_out(i:))
      pos(1) = pos(1) + i - 1
      if (pos(1) /= i) then
        tmp = array_out(i)
        array_out(i) = array_out(pos(1))
        array_out(pos(1)) = tmp
      end if
    end do
  end subroutine sortlowhigh


  ! Destructive in-place descending (selection with maxloc on shrinking slices)
  subroutine sorthighlow_inplace(a)
    real(kind=rkind), intent(inout), dimension(:) :: a
    integer(kind=ikind) :: n, i
    integer(kind=ikind), dimension(1) :: pos
    real(kind=rkind) :: tmp

    n = size(a)
    if (n <= 1) return

    do i = 1, n
      pos = maxloc(a(i:))
      pos(1) = pos(1) + i - 1
      if (pos(1) /= i) then
        tmp = a(i)
        a(i) = a(pos(1))
        a(pos(1)) = tmp
      end if
    end do
  end subroutine sorthighlow_inplace


  ! Destructive in-place ascending (selection with minloc)
  subroutine sortlowhigh_inplace(a)
    real(kind=rkind), intent(inout), dimension(:) :: a
    integer(kind=ikind) :: n, i
    integer(kind=ikind), dimension(1) :: pos
    real(kind=rkind) :: tmp

    n = size(a)
    if (n <= 1) return

    do i = 1, n
      pos = minloc(a(i:))
      pos(1) = pos(1) + i - 1
      if (pos(1) /= i) then
        tmp = a(i)
        a(i) = a(pos(1))
        a(pos(1)) = tmp
      end if
    end do
  end subroutine sortlowhigh_inplace

end module sortnumbers
