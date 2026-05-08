module m_test_helpers
    use testdrive, only : new_unittest, unittest_type, error_type, check

    use t_depac_meteorology, only: depac_meteorology
    use m_helpers, only: fpsih, missing


    implicit none (type, external)
    private
    public :: collect_test_helpers_tests, test_fpsih
    contains
    subroutine collect_test_helpers_tests(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("fpsih", test_fpsih) &
        ]

    end subroutine collect_test_helpers_tests

    subroutine test_fpsih(error)
        type(error_type), allocatable, intent(out) :: error
        real :: x

        x = 0.0

        call check(error, fpsih(x), 0.0, message="fpsih(0) test failed", thr=1.0e-5)
        if (allocated(error)) return

        x = 1.0
        call check(error, fpsih(x), -4.39425993, message="fpsih(1) test failed", thr=1.0e-5)
        if (allocated(error)) return

        x = -1.0
        call check(error, fpsih(x), 1.88122725, message="fpsih(-1) test failed", thr=1.0e-5)
        if (allocated(error)) return

    end subroutine test_fpsih




end module m_test_helpers