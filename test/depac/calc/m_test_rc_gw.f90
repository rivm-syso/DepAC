module m_test_rc_gw
    use testdrive, only : new_unittest, unittest_type, error_type, check

    use m_test_gw_param, only: collect_gw_param_tests


    implicit none (type, external)
    private
    public :: collect_rc_gw_tests
    contains

    subroutine collect_rc_gw_tests(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        call collect_gw_param_tests(testsuite)

        testsuite = [testsuite, &
            new_unittest("rc_gw", test_rc_gw) &
        ]

    end subroutine collect_rc_gw_tests
    subroutine test_rc_gw(error)
        type(error_type), allocatable, intent(out) :: error
        ! TODO implement test for the main rc_gw function



    end subroutine test_rc_gw


end module m_test_rc_gw
