module m_test_rc_gstom
    use testdrive, only : new_unittest, unittest_type, error_type, check

    use m_test_gstom_param, only: collect_gstom_param_tests

    implicit none (type, external)
    private
    public :: collect_rc_gstom_tests
    contains

    subroutine collect_rc_gstom_tests(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        call collect_gstom_param_tests(testsuite)

        testsuite = [testsuite, &
            new_unittest("rc_gstom", test_rc_gstom) &
        ]


    end subroutine collect_rc_gstom_tests

    subroutine test_rc_gstom(error)
        type(error_type), allocatable, intent(out) :: error
        ! TODO implement test for the main rc_gstom function


    end subroutine test_rc_gstom


end module m_test_rc_gstom