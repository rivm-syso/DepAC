module m_test_comp_points
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use m_test_comp_point_param, only: collect_comp_point_param_tests

    implicit none (type, external)
    private
    public :: collect_comp_points_tests
    contains


    subroutine collect_comp_points_tests(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        call collect_comp_point_param_tests(testsuite)


        testsuite = [testsuite, &
            new_unittest("rc_comp_point", test_rc_comp_point) &
        ]

    end subroutine collect_comp_points_tests

    subroutine test_rc_comp_point(error)
        type(error_type), allocatable, intent(out) :: error

        ! TODO Write a test testing the general function rc_comp_point


    end subroutine test_rc_comp_point

end module m_test_comp_points