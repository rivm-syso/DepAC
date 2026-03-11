module m_test_rc_gsoil
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use t_depac_component, only: depac_component
    use t_depac_land_use, only: depac_land_use
    use t_depac_meteorology, only: depac_meteorology
    use t_depac_config, only: depac_config
    use t_depac_output, only: depac_output
    use t_depac_error, only: depac_error, ERR_INPUT
    use m_logger, only: set_log_level, LOG_LEVEL_NONE
    use m_helpers, only: missing
    use m_rc_gsoil, only: rc_gsoil, rc_rinc
    use m_depac_error, only: set_error
    use default_indices, only: LU_GRASS, LU_OTHER, LU_ARABLE

    implicit none (type, external)
    private
    public :: collect_rc_gsoil_tests
    contains

    subroutine collect_rc_gsoil_tests(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("rc_gsoil", test_rc_gsoil), &
            new_unittest("rc_rinc", test_rc_rinc) &
        ]

    end subroutine collect_rc_gsoil_tests

    subroutine test_rc_gsoil(error)
        type(error_type), allocatable, intent(out) :: error

        type(depac_component) :: comp
        type(depac_land_use) :: lu
        type(depac_meteorology) :: meteo
        type(depac_config) :: dp_conf
        type(depac_output) :: dp_out
        type(depac_error) :: dp_err
        logical :: ready

        call set_log_level(LOG_LEVEL_NONE)

        ! Add test cases for rc_gsoil here





        ! setup such that rinc = -999
        lu%rc_rinc%b = -1.0

        lu%rc_rinc%h = 1
        meteo%ust = 0.5
        dp_conf%sai = 1.5
        lu%name = "grass"
        lu%index = LU_GRASS

        call rc_gsoil(lu, meteo, comp, dp_conf, dp_out, dp_err)
        call check(error, dp_out%gsoil_eff, 0.0, message="missing(rinc) failed", thr=1.0e-5)
        if (allocated(error)) return

        ! frozen soil
        lu%rc_rinc%b = 1.0
        meteo%t = -5.0 ! negative temperature
        comp%rsoil_frozen = 1.0

        call rc_gsoil(lu, meteo, comp, dp_conf, dp_out, dp_err)
        call check(error, dp_out%gsoil_eff, 0.25,&
         message="r_soil_frozen + rinc failed", thr=1.0e-5)
        if (allocated(error)) return

        ! frozen soil, missing comp%rsoil_frozen
        comp%rsoil_frozen = -999.0

        call rc_gsoil(lu, meteo, comp, dp_conf, dp_out, dp_err)
        call check(error, dp_out%gsoil_eff, 0.0,&
         message="missing comp%rsoil_frozen failed", thr=1.0e-5)
        if (allocated(error)) return

        ! non-frozen soil, dry
        meteo%t = 5.0 ! positive temperature
        meteo%nwet = 0
        lu%rsoil = 4.0

        call rc_gsoil(lu, meteo, comp, dp_conf, dp_out, dp_err)
        call check(error, dp_out%gsoil_eff, 0.142857149,&
         message="missing comp%rsoil_frozen failed", thr=1.0e-5)
        if (allocated(error)) return

        ! non-frozen soil, dry, missing lu%rsoil
        lu%rsoil = -999.0
        call rc_gsoil(lu, meteo, comp, dp_conf, dp_out, dp_err)
        call check(error, dp_out%gsoil_eff, 0.0, &
        message="missing lu%rsoil failed", thr=1.0e-5)
        if (allocated(error)) return

        ! non-frozen soil, wet
        meteo%nwet = 1
        comp%rsoil_wet = 2.0

        call rc_gsoil(lu, meteo, comp, dp_conf, dp_out, dp_err)
        call check(error, dp_out%gsoil_eff, 0.20,&
            message="missing comp%rsoil_wet failed", thr=1.0e-5)
        if (allocated(error)) return

        ! non-frozen soil, wet, missing comp%rsoil_wet
        comp%rsoil_wet = -999.0
        call rc_gsoil(lu, meteo, comp, dp_conf, dp_out, dp_err)
        call check(error, dp_out%gsoil_eff, 0.0, &
            message="missing comp%rsoil_wet failed", thr=1.0e-5)
        if (allocated(error)) return

        meteo%nwet = 2 ! invalid nwet
        call rc_gsoil(lu, meteo, comp, dp_conf, dp_out, dp_err)
        call check(error, dp_err%code, ERR_INPUT, &
            message="invalid nwet did not set error")
        if (allocated(error)) return

    end subroutine test_rc_gsoil

    subroutine test_rc_rinc(error)
        type(error_type), allocatable, intent(out) :: error

        type(depac_land_use) :: lu
        type(depac_meteorology) :: meteo
        type(depac_config) :: dp_conf
        type(depac_output) :: dp_out
        real :: rinc

        call set_log_level(LOG_LEVEL_NONE)

        ! arable land
        lu%rc_rinc%b = 1.0
        lu%rc_rinc%h = 1.0
        meteo%ust = -0.5
        dp_conf%sai = 1.5
        lu%name = "grass"
        lu%index = LU_GRASS

        call rc_rinc(lu, meteo, dp_conf, dp_out, rinc)
        call check(error, rinc, 1000.0, message="b >0 and ust <= 0 failed", thr=1.0e-5)
        if (allocated(error)) return
        meteo%ust = 0.5

        call rc_rinc(lu, meteo, dp_conf, dp_out, rinc)
        call check(error, rinc, 3.0, message="b >0 and ust > 0 failed", thr=1.0e-5)
        if (allocated(error)) return

        lu%rc_rinc%b = -0.5

        call rc_rinc(lu, meteo, dp_conf, dp_out, rinc)
        call check(error, rinc, -999.0, message="b < 0 grass lu failed", thr=1.0e-5)
        if (allocated(error)) return

        lu%name = "other"
        lu%index = LU_OTHER

        call rc_rinc(lu, meteo, dp_conf, dp_out, rinc)
        call check(error, rinc, -999.0, message="b < 0 other lu failed", thr=1.0e-5)
        if (allocated(error)) return

        lu%name = "arable"
        lu%index = LU_ARABLE

        call rc_rinc(lu, meteo, dp_conf, dp_out, rinc)
        call check(error, rinc, 0.0, message="b < 0 and any land use failed", thr=1.0e-5)
        if (allocated(error)) return

    end subroutine test_rc_rinc
end module m_test_rc_gsoil