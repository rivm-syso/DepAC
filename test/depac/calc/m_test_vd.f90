module m_test_vd
     use testdrive, only : new_unittest, unittest_type, error_type, check

    use t_depac_context, only: depac_context
    use m_vd, only: depac_calc_vd_tot, depac_calc_vd_eff
    implicit none (type, external)
    private
    public :: collect_vd_tests
contains

    subroutine collect_vd_tests(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("calc_vd_tot", test_calc_vd_tot), &
            new_unittest("calc_vd_eff", test_calc_vd_eff) &
        ]

    end subroutine collect_vd_tests

    subroutine test_calc_vd_tot(error)
        type(error_type), allocatable, intent(out) :: error
        type(depac_context) :: ctx
        real :: vd

        ! Set up context with valid values
        ctx%state%ra_obs = 200.0
        ctx%state%rb = 5.0
        ctx%output%rc_tot = 15.0

        vd = depac_calc_vd_tot(ctx)
        call check(error, vd, 4.54545440E-03, &
             message="vd_tot calculation failed", thr=1.0e-5)

        ! Test with zero denominator
        ctx%state%ra_obs = 5.0
        ctx%state%rb = 5.0
        ctx%output%rc_tot = -10.0

        vd = depac_calc_vd_tot(ctx)
        call check(error, vd, -999.0, &
             message="vd_tot calculation with zero denominator failed", thr=1.0e-5)
    end subroutine test_calc_vd_tot

    subroutine test_calc_vd_eff(error)
        type(error_type), allocatable, intent(out) :: error
        type(depac_context) :: ctx
        real :: vd_eff

        ! Set up context with valid values
        ctx%state%ra_obs = 10.0
        ctx%state%rb = 5.0
        ctx%output%rc_eff = 15.0

        vd_eff = depac_calc_vd_eff(ctx)

        call check(error, vd_eff, 3.33333351E-02, &
             message="vd_eff calculation failed", thr=1.0e-5)

        ! Test with zero denominator
        ctx%state%ra_obs = 5.0
        ctx%state%rb = 5.0
        ctx%output%rc_eff = -10.0

        vd_eff = depac_calc_vd_eff(ctx)
        call check(error, vd_eff, -999.0, &
             message="vd_eff calculation with zero denominator failed", thr=1.0e-5)
    end subroutine test_calc_vd_eff

end module m_test_vd