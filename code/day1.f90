module sort

contains

subroutine Shell_Sort(a)

    implicit none
    integer :: i, j, increment
    integer :: temp
    integer, intent(in out) :: a(:)

    increment = size(a) / 2
    do while (increment > 0)
        do i = increment+1,size(a)
            j = i
            temp = a(i)
            do while (j>= increment+1 .and. a(j-increment)>temp)
                a(j) = a(j-increment)
                j = j-increment
            end do
            a(j) = temp
        end do
        if (increment == 2) then
            increment = 1
        else
            increment = increment * 5 / 11
        end if
    end do

end subroutine Shell_Sort

end module sort


program advent_day1

use sort

    implicit none

    integer :: iunit, i, diffsum, n
    !logical :: status_ok
!    character(len=:),allocatable :: line
    integer,dimension(:),allocatable :: col1,col2

!    iunit = 1
    open(newunit=iunit,file='inputs/day1.txt',status='old',action='read')
    n = 1000      ! lines in file. fix to read by itself if time.
    allocate(col1(n),col2(n))
    do i = 1, n
        read(iunit, *) col1(i), col2(i)
    end do
    close(iunit)

    ! part 1
    call Shell_Sort(col1)
    call Shell_Sort(col2)
    diffsum = sum(abs(col2-col1))
    write(*,*) '1a: sum=', diffsum


end program