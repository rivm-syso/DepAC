module m_test_rb
    use testdrive, only : new_unittest, unittest_type, error_type, check

    use t_depac_context, only: depac_context
    use t_depac_setup, only: depac_setup
    use m_rb, only: depac_calc_rb_hicks


    implicit none (type, external)
    private
    public :: collect_calc_rb_tests
    contains

    subroutine collect_calc_rb_tests(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("calc_rb", test_calc_rb) &
        ]

    end subroutine collect_calc_rb_tests

    subroutine test_calc_rb(error)
        type(error_type), allocatable, intent(out) :: error
        ! internal variables
        type(depac_context) :: ctx
        type(depac_setup) :: setup
        real :: rb

        ctx%meteo%ust = -0.1
        setup%component%diffc = 0.2e-4

        rb = depac_calc_rb_hicks(setup, ctx)
        call check(error, rb, -999.0,&
             message="rb calculation with invalid ust failed", thr=1.0e-5)


        ctx%meteo%ust = 0.5
        setup%component%diffc = -0.1e-4
        rb = depac_calc_rb_hicks(setup, ctx)
        call check(error, rb, -999.0,&
             message="rb calculation with invalid diffc failed", thr=1.0e-5)

        ctx%meteo%ust = 0.5
        setup%component%diffc = 0.1e-4
        rb = depac_calc_rb_hicks(setup, ctx)

        call check(error, rb, 15.9107304,&
             message="rb calculation failed", thr=1.0e-5)
    end subroutine test_calc_rb
end module m_test_rb
