module m_test_rc_eff
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use t_depac_context, only: depac_context
    use t_depac_setup, only: depac_setup
    use m_rc_eff, only: rc_eff

    use m_logger, only: set_log_level, LOG_LEVEL_NONE
    implicit none (type, external)
    private
    public :: collect_rc_eff_tests
    contains

    subroutine collect_rc_eff_tests(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("rc_eff", test_rc_eff) &
        ]

    end subroutine collect_rc_eff_tests

    subroutine test_rc_eff(error)
        type(error_type), allocatable, intent(out) :: error

        type(depac_setup) :: setup
        type(depac_context) :: ctx

        call set_log_level(LOG_LEVEL_NONE)

        setup%config%comp_point%c_nh3 = 10.0
        ctx%output%ccomp_tot = 2.0

        setup%config%ra_obs = 5.0
        setup%config%rb = 3.0
        ctx%output%rc_tot = 4.0

        call rc_eff(setup, ctx)
        call check(error, ctx%output%rc_eff, 7.0,&
             message="rc_eff normal case failed", thr=1.0e-5)
        if (allocated(error)) return

        ! comp_tot equals c_nh3 case
        ctx%output%ccomp_tot = 10.0
        call rc_eff(setup, ctx)
        call check(error, ctx%output%rc_eff, -9999.0,&
             message="rc_eff equal comp_tot case failed", thr=1.0e-5)
        if (allocated(error)) return
    end subroutine test_rc_eff
end module m_test_rc_eff