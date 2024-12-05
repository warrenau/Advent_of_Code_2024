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

!**************************************
program advent_day1

use sort
implicit none

    integer, dimension(:), allocatable :: p, q
    integer :: i, n, diffsum

    n = 1000
    allocate(p(n), q(n))

    ! read data from a file
    open(2, file='inputs/day1.txt', status='old')
    do i=1,n
        read(2,*) p(i), q(i)
    end do
    close(2)

    ! sort each array
    call Shell_Sort(p)
    call Shell_Sort(q)

    ! write data to new file
    open(3, file='inputs/day1_2.txt', status='new')
    do i=1,n
        write(3,*) p(i), q(i)
    end do
    close(3)

    diffsum = sum(abs(q-p))
    write(*,*) '1a: sum=', diffsum
end program advent_day1
