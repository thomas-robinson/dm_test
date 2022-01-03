module dob
use datamod
implicit none

type diagObject
  integer :: i
  class (diagData_Type),allocatable :: diag_data
contains
  procedure :: registerOjb
  procedure :: send_data_obj
end type diagObject

type(diagObject) :: obj


contains

function registerOjb (ob,i) result (used)
class(diagObject) :: ob
integer :: i
logical :: used
ob%i = i
used = .true.
end function registerOjb

function send_data_obj (ob) result (used)
class(diagObject) :: ob
logical :: used

used = .true.
! select case (ob%dim_of_data)
! case (0)
!         write (6,*) "scalar"
! case (1)
!         write (6,*) "1d"
! case (2)
!         write (6,*) "2d"
! case (3)
!         write (6,*) "3d"
! case (4)
!         write (6,*) "4d"
! case (5)
!         write (6,*) "5d"
! case default
!         write (6,*) "no data"
!         used = .false.
! end select

end function send_data_obj

end module dob
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module datamod
type diagData_Type
  integer :: dim_of_data = -1
end type diagData_Type

type, extends (diagData_Type) :: diagData0
  real, allocatable :: diag_data0d
end type diagData0
type, extends (diagData_Type) :: diagData1
  real, allocatable, dimension(:) :: diag_data1d
end type diagData1
type, extends (diagData_Type) :: diagData2
  real, allocatable, dimension(:,:) :: diag_data2d
end type diagData2
type, extends (diagData_Type) :: diagData3
  real, allocatable, dimension(:,:,:) :: diag_data3d
end type diagData3
type, extends (diagData_Type) :: diagData4
  real, allocatable, dimension(:,:,:,:) :: diag_data4d
end type diagData4
type, extends (diagData_Type) :: diagData5
  real, allocatable, dimension(:,:,:,:,:) :: diag_data5d
end type diagData5        
end module datamod
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module funcs
use alloc
use datamod
implicit none

interface send_data
        module procedure send_5d
        module procedure send_4d
        module procedure send_3d
        module procedure send_2d
        module procedure send_1d
        module procedure send_0d
end interface 
contains
logical function register (i) 
integer :: i
register = obj%registerOjb(i)
end function register

logical function send_5d (diag_data) 
  real, dimension (:,:,:,:,:) :: diag_data
!  if (.not. allocated(obj%diag_data)) then
        obj%diag_data = diagData5()
        allocate(obj%diag_data%diag_data5d(size(diag_data,1),size(diag_data,2),size(diag_data,3),&
                 size(diag_data,4),size(diag_data,5)))
         obj%diag_data%dim_of_data = 5
!  endif
  send_5d = obj%send_data_obj()
end function send_5d
logical function send_4d (diag_data)
  real, dimension (:,:,:,:) :: diag_data
  if (.not. allocated(obj%diag_data%diag_data4d)) then
        allocate(obj%diag_data%diag_data4d(size(diag_data,1),size(diag_data,2),size(diag_data,3),&
                 size(diag_data,4)))
         obj%diag_data%dim_of_data = 4
  endif
  send_4d = obj%send_data_obj()
end function send_4d
logical function send_3d (diag_data)
  real, dimension (:,:,:) :: diag_data
  if (.not. allocated(obj%diag_data%diag_data3d)) then
        allocate(obj%diag_data%diag_data3d(size(diag_data,1),size(diag_data,2),size(diag_data,3)))
         obj%diag_data%dim_of_data = 3
  endif
  send_3d = obj%send_data_obj()
end function send_3d
logical function send_2d (diag_data)
  real, dimension (:,:) :: diag_data
  if (.not. allocated(obj%diag_data%diag_data2d)) then
        allocate(obj%diag_data%diag_data2d(size(diag_data,1),size(diag_data,2)))
         obj%diag_data%dim_of_data = 2
  endif
  send_2d = obj%send_data_obj()
end function send_2d
logical function send_1d (diag_data)
  real, dimension (:) :: diag_data
  if (.not. allocated(obj%diag_data%diag_data1d)) then
        allocate(obj%diag_data%diag_data1d(size(diag_data,1)))
         obj%diag_data%dim_of_data = 1
  endif
  send_1d = obj%send_data_obj()
end function send_1d
logical function send_0d (diag_data)
  real :: diag_data
  if (.not. allocated(obj%diag_data%diag_data0d)) then
        allocate(obj%diag_data%diag_data0d)
         obj%diag_data%dim_of_data = 0
  endif
  send_0d = obj%send_data_obj()
end function send_0d

end module funcs

program testalloc
use alloc
use funcs
use datamod
implicit none

integer :: i = 10
logical :: used
real :: diag_data (4,3,2,1)
used = register (i)
write (6,*) used
used = .false.
diag_data = 1.1
used = send_data(diag_data)
write (6,*) obj%diag_data%dim_of_data
end program testalloc
