module m_test_rc_tot
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use t_depac_output, only: depac_output
    use t_depac_error, only: depac_error, ERR_INPUT
    use m_logger, only: set_log_level, LOG_LEVEL_NONE
    use m_rc_tot, only: rc_tot
    use m_depac_error, only: set_error

    implicit none (type, external)
    private
    public :: collect_rc_tot_tests
    contains

    subroutine collect_rc_tot_tests(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("rc_tot", test_rc_tot) &
        ]

    end subroutine collect_rc_tot_tests

    subroutine test_rc_tot(error)
        type(error_type), allocatable, intent(out) :: error

        type(depac_output) :: dp_out
        type(depac_error) :: dp_err
        logical :: ready

        call set_log_level(LOG_LEVEL_NONE)

        dp_out%gstom = 2.0
        dp_out%gsoil_eff = 3.0
        dp_out%gw = 4.0

        call rc_tot(dp_out, dp_err)
        call check(error, dp_out%rc_tot, 0.111111112,&
             message="rc_tot normal case failed", thr=1.0e-5)
        if (allocated(error)) return

        ! gw negative case
        dp_out%gw = -1.0
        call rc_tot(dp_out, dp_err)
        call check(error, dp_out%rc_tot, -9999.0, &
            message="rc_tot negative gw case failed", thr=1.0e-5)
        if (allocated(error)) return

        ! gc_tot negative case
        dp_out%gstom = -5.0
        dp_out%gw = 1.0
        dp_out%gsoil_eff = 1.0
        call rc_tot(dp_out, dp_err)
        call check(error, dp_out%rc_tot, -9999.0, &
            message="rc_tot negative gc_tot case failed", thr=1.0e-5)
        if (allocated(error)) return



    end subroutine test_rc_tot
end module m_test_rc_tot