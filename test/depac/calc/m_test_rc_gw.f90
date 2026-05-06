module m_test_rc_gw
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use t_depac_component, only: depac_component
    use t_depac_land_use, only: depac_land_use
    use t_depac_meteorology, only: depac_meteorology
    use t_depac_config, only: depac_config
    use t_depac_output, only: depac_output
    use t_depac_error, only: depac_error, ERR_INPUT
    use m_logger, only: set_log_level, LOG_LEVEL_NONE
    use m_helpers, only: missing
    use m_rc_gw, only: rc_gw
    use m_depac_error, only: set_error
    use default_indices, only: COMP_NO, COMP_NO2, COMP_O3, COMP_SO2, COMP_NH3

    implicit none (type, external)
    private
    public :: collect_rc_gw_tests
    contains

    subroutine collect_rc_gw_tests(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("rc_gw", test_rc_gw) &
        ]

    end subroutine collect_rc_gw_tests
    subroutine test_rc_gw(error)
        type(error_type), allocatable, intent(out) :: error

        type(depac_component) :: comp
        type(depac_land_use) :: lu
        type(depac_meteorology) :: meteo
        type(depac_config) :: dp_conf
        type(depac_output) :: dp_out
        type(depac_error) :: dp_err
        logical :: ready

        call set_log_level(LOG_LEVEL_NONE)
        ! missing rw_val test
        comp%name = "NO2"
        comp%index = COMP_NO2
        comp%rw_val = -999.0
        call rc_gw(comp, meteo, dp_conf, dp_out, dp_err)
        call check(error, dp_out%gw, -999.0, &
            message="rc_gw missing rw_val test failed", thr=1.0e-5)
        if (allocated(error)) return

        ! non-existing component
        comp%name = "XYZ"
        comp%index = 100
        call rc_gw(comp, meteo, dp_conf, dp_out, dp_err)
        call check(error, dp_err%code, ERR_INPUT, &
            message="rc_gw non-existing component test failed")
        if (allocated(error)) return

        comp%index = COMP_NO2
        comp%rw_val = 100.0
        dp_conf%has_vegetation = .true.
        call rc_gw(comp, meteo, dp_conf, dp_out, dp_err)
        call check(error, dp_out%gw, 0.01, &
            message="rc_gw NO with vegetation test failed", thr=1.0e-5)
        if (allocated(error)) return
        dp_conf%has_vegetation = .false.
        call rc_gw(comp, meteo, dp_conf, dp_out, dp_err)
        call check(error, dp_out%gw, 0.0, &
            message="rc_gw NO with no vegetation test failed", thr=1.0e-5)
        if (allocated(error)) return

        comp%name = "SO2"
        comp%index = COMP_SO2
        dp_conf%has_vegetation = .true.
        meteo%nwet = 0
        meteo%t = 0.
        meteo%rh = 80.
        dp_conf%comp_point%iratns = 1

        call rc_gw(comp, meteo, dp_conf, dp_out, dp_err)
        call check(error, dp_out%gw, 1.0227e-2, &
            message="rc_gw SO2 with vegetation test failed", thr=1.0e-5)
        if (allocated(error)) return





    end subroutine test_rc_gw

    subroutine test_rw_so2(error)
        type(error_type), allocatable, intent(out) :: error
        type(depac_component) :: comp_p
        type(depac_land_use) :: lu
        type(depac_meteorology) :: meteo
        type(depac_config) :: dp_conf
        type(depac_output) :: dp_out
        type(depac_error) :: dp_err
        logical :: ready

        call set_log_level(LOG_LEVEL_NONE)

        ! Test: has_vegetation true, nwet = 0
        dp_conf%has_vegetation = .true.
        meteo%nwet = 0
        meteo%t = 0.
        meteo%rh = 80.
        dp_conf%comp_point%iratns = 1

        call rw_so2(meteo, dp_conf, dp_out, dp_err)
        call check(error, dp_out%gw, 1.0227e-2, message="rw_so2 with nwet=0 failed", thr=1.0e-5)
        if (allocated(error)) return
        ! Test: rh > 81.3
        meteo%rh = 90.
        call rw_so2(meteo, dp_conf, dp_out, dp_err)
        call check(error, dp_out%gw, 5.58799e-2, &
            message="rw_so2 with nwet=0 and rh > 81.3 failed", thr=1.0e-5)
        if (allocated(error)) return

        ! Test: T < -1
        meteo%t = -2.
        call rw_so2(meteo, dp_conf, dp_out, dp_err)
        call check(error, dp_out%gw, 5e-3, &
            message="rw_so2 with nwet=0 and T < -1 failed", thr=1.0e-5)
        if (allocated(error)) return

        ! Test: T < -5
        meteo%t = -5.3
        call rw_so2(meteo, dp_conf, dp_out, dp_err)
        call check(error, dp_out%gw, 2e-3, &
            message="rw_so2 with nwet=0 and T < -5 failed", thr=1.0e-5)
        if (allocated(error)) return

        ! Test: nwet = 1
        meteo%nwet = 1
        call rw_so2(meteo, dp_conf, dp_out, dp_err)
        call check(error, dp_out%gw, 0.1, &
            message="rw_so2 with nwet=1 failed", thr=1.0e-5)
        if (allocated(error)) return

        ! Test: iratns = 3
        dp_conf%comp_point%iratns = 3
        call rw_so2(meteo, dp_conf, dp_out, dp_err)
        call check(error, dp_out%gw, 1.66666e-2, &
            message="rw_so2 with iratns=3 failed", thr=1.0e-5)
        if (allocated(error)) return

        ! Test: no vegetation
        dp_conf%has_vegetation = .false.
        call rw_so2(meteo, dp_conf, dp_out, dp_err)
        call check(error, dp_out%gw, 0.0, message="rw_so2 with no vegetation failed", thr=1.0e-5)
        if (allocated(error)) return

    end subroutine test_rw_so2


    subroutine test_rw_nh3_sutton(error)
        type(error_type), allocatable, intent(out) :: error
        type(depac_meteorology) :: meteo
        type(depac_config) :: dp_conf
        type(depac_output) :: dp_out

        call set_log_level(LOG_LEVEL_NONE)

        ! Test: vegetation, non-frozen soil
        dp_conf%has_vegetation = .true.
        dp_conf%sai = 0.6
        dp_conf%sai_grass_haarweg = 0.5 ! typical value for test
        meteo%rh = 80.
        meteo%tsurf = 0.1

        call rw_nh3_sutton(meteo, dp_conf, dp_out)
        call check(error, dp_out%gw, 3.00000003E-03, &
            message="rw_nh3_sutton with vegetation", thr=1.0e-5)
        if (allocated(error)) return

        ! Test: frozen soil
        meteo%tsurf = -2.0
        call rw_nh3_sutton(meteo, dp_conf, dp_out)
        call check(error, dp_out%gw, dp_conf%sai / 200.0, &
            message="rw_nh3_sutton with frozen soil", thr=1.0e-5)
        if (allocated(error)) return

        ! Test: no vegetation
        dp_conf%has_vegetation = .false.
        call rw_nh3_sutton(meteo, dp_conf, dp_out)
        call check(error, dp_out%gw, 0.0, message="rw_nh3_sutton with no vegetation", thr=1.0e-5)
        if (allocated(error)) return

    end subroutine test_rw_nh3_sutton

end module m_test_rc_gw