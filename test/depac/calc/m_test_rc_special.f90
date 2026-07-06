module m_test_rc_special
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use m_test_rc_special_param, only: collect_rc_special_param_tests

    implicit none (type, external)
    private
    public :: collect_rc_special_tests
    contains

    subroutine collect_rc_special_tests(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        call collect_rc_special_param_tests(testsuite)
        testsuite = [testsuite, &
            new_unittest("rc_special", test_rc_special) &
        ]

    end subroutine collect_rc_special_tests



    subroutine test_rc_special(error)
        type(error_type), allocatable, intent(out) :: error
        ! TODO implement test for the main rc_special function

    end subroutine test_rc_special
end module m_test_rc_special
