program test_sort
  use types
  use sortnumbers
  implicit none

  real(kind=rkind), allocatable, dimension(:) :: v, vcopy, out
  integer(kind=ikind) :: n, i

  ! prepare a test vector with positive, negative, duplicate and large values
  n = 7
  allocate(v(n))
  v = [ 3.5_rkind, -2.1_rkind, 1.0e6_rkind, 0.0_rkind, -7.2_rkind, 3.5_rkind, 42.0_rkind ]

  print *, 'Original vector:'
  do i = 1, n
    print *, i, v(i)
  end do

  ! Non-destructive descending
  call sorthighlow(v, out)
  print *, 'Non-destructive descending (sorthighlow):'
  do i = 1, n
    print *, i, out(i)
  end do

  print *, 'Verify original unchanged (should match first output):'
  do i = 1, n
    print *, i, v(i)
  end do

  ! In-place descending: operate on a copy so original stays for further tests
  allocate(vcopy(n))
  vcopy = v
  call sorthighlow_inplace(vcopy)
  print *, 'In-place descending (sorthighlow_inplace), first 5 elements:'
  do i = 1, min(n,5)
    print *, i, vcopy(i)
  end do

  ! Non-destructive ascending
  if (allocated(out)) deallocate(out)
  call sortlowhigh(v, out)
  print *, 'Non-destructive ascending (sortlowhigh):'
  do i = 1, n
    print *, i, out(i)
  end do

  ! In-place ascending
  vcopy = v
  call sortlowhigh_inplace(vcopy)
  print *, 'In-place ascending (sortlowhigh_inplace), first 5 elements:'
  do i = 1, min(n,5)
    print *, i, vcopy(i)
  end do

  ! tidy up
  deallocate(v)
  deallocate(vcopy)
  if (allocated(out)) deallocate(out)
end program test_sort
