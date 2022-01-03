module ex
      implicit none

      type diagObject
              integer :: i
      end type diagObject

      type, extends (diagObject) :: diagObject3d
              integer :: d3 
              real, allocatable, dimension (:,:,:) :: diag_data3
      end type diagObject3d
      type, extends (diagObject) :: diagObject2d
              integer :: d2
              real, allocatable, dimension (:,:) :: diag_data2
      end type diagObject2d
      type, extends (diagObject) :: diagObject1d
              integer :: d1
              real, allocatable, dimension (:) :: diag_data1
      end type diagObject1d
      type, extends (diagObject) :: diagObject0d
              integer :: d0
              real :: diag_data0
      end type diagObject0d


      class(diagObject),allocatable :: obj

      contains

      subroutine register (i)
              integer :: i

              obj%i = i
      end subroutine register

      function send_3d (diag_data) result (used)
              real, dimension (:,:,:) :: diag_data
              logical :: used
              obj%d3 = 3
              allocate (obj%diag_data3(size(diag_data,1),size(diag_data,2),size(diag_data,3)))
              obj%diag_data3 = diag_data
              used = .true.
       end function send_3d
      function send_2d (diag_data) result (used)
              real, dimension (:,:) :: diag_data
              logical :: used
              obj%d2 = 2
              allocate (obj%diag_data2(size(diag_data,1),size(diag_data,2)))
              obj%diag_data2 = diag_data
              used = .true.
       end function send_2d

end module ex


program extest
use ex
      implicit none

      integer :: i = 10

      call register (i)
end program extest
