module m_test_version
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use m_version, only: VERSION, BUILD_DATE

    implicit none (type, external)
    private
    public :: collect_version_tests
    contains

    subroutine collect_version_tests(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("test_version", test_version) &
        ]

    end subroutine collect_version_tests


    subroutine test_version(error)
        type(error_type), allocatable, intent(out) :: error

        ! check the file VERSION
        character(len=20) :: line
        integer :: ios

        open(unit=10, file="VERSION", status="old", action="read", iostat=ios)

        call check(error, ios, 0, message="Failed to open VERSION file")
        if (allocated(error)) return

        read(10, "(A)", iostat=ios) line

        call check(error, ios, 0, message="Failed to read VERSION file")
        if (allocated(error)) return

        call check(error, trim(line) == VERSION, .true.,&
             message="VERSION file does not match m_version VERSION")
        if (allocated(error)) return


    end subroutine test_version

end module m_test_version
