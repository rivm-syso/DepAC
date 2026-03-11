module m_test_rc_gstom
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use t_depac_component, only: depac_component
    use t_depac_land_use, only: depac_land_use
    use t_depac_meteorology, only: depac_meteorology
    use t_depac_config, only: depac_config
    use t_depac_output, only: depac_output
    use t_depac_error, only: depac_error, ERR_INPUT
    use m_logger, only: set_log_level, LOG_LEVEL_NONE
    use m_helpers, only: missing
    use m_rc_gstom, only: rc_gstom, rc_get_vpd, par_dir_diff, rc_gstom_emb
    use m_depac_error, only: set_error
    use default_indices, only: COMP_NO, COMP_NO2, COMP_O3, COMP_SO2, COMP_NH3
    implicit none (type, external)
    private
    public :: collect_rc_gstom_tests
    contains

    subroutine collect_rc_gstom_tests(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("rc_gstom", test_rc_gstom), &
            new_unittest("get_vpd", test_get_vpd), &
            new_unittest("par_dir_diff", test_par_dir_diff), &
            new_unittest("rc_gstom_emb", test_rc_gstom_emb) &
        ]

    end subroutine collect_rc_gstom_tests

    subroutine test_rc_gstom(error)
        type(error_type), allocatable, intent(out) :: error

        type(depac_component) :: comp
        type(depac_land_use) :: lu
        type(depac_meteorology) :: meteo
        type(depac_config) :: dp_conf
        type(depac_output) :: dp_out
        type(depac_error) :: dp_err
        logical :: ready


        call set_log_level(LOG_LEVEL_NONE)

        ! Add test cases for rc_gstom here

        ! test comp is NO
        comp%name = "NO"
        comp%index = COMP_NO

        call rc_gstom(comp, lu, meteo, dp_conf, dp_out, dp_err)

        call check(error, dp_out%gstom, 0.0, message="rc_gstom with comp=NO failed", thr=1.0e-5)
        if (allocated(error)) return

        ! test comp is unsupported
        comp%name = "UNSUPPORTED_COMP"
        comp%index = 999
        call rc_gstom(comp, lu, meteo, dp_conf, dp_out, dp_err)
        call check(error, dp_err%code, ERR_INPUT,&
            message="rc_gstom with unsupported comp did not set error")
        if (allocated(error)) return

        ! comp is NO2, O3, SO2 or NH3:

        comp%name = "NO2"
        comp%index = COMP_NO2
        dp_conf%has_vegetation = .false.
        call rc_gstom(comp, lu, meteo, dp_conf, dp_out, dp_err)
        call check(error, dp_out%gstom, 0.0, message="rc_gstom with comp=NO2 failed", thr=1.0e-5)
        if (allocated(error)) return

        comp%name = "O3"
        comp%index = COMP_O3
        call rc_gstom(comp, lu, meteo, dp_conf, dp_out, dp_err)
        call check(error, dp_out%gstom, 0.0, message="rc_gstom with comp=O3 failed", thr=1.0e-5)
        if (allocated(error)) return

        comp%name = "SO2"
        comp%index = COMP_SO2
        call rc_gstom(comp, lu, meteo, dp_conf, dp_out, dp_err)
        call check(error, dp_out%gstom, 0.0, message="rc_gstom with comp=SO2 failed", thr=1.0e-5)
        if (allocated(error)) return

        comp%name = "NH3"
        comp%index = COMP_NH3
        call rc_gstom(comp, lu, meteo, dp_conf, dp_out, dp_err)
        call check(error, dp_out%gstom, 0.0, message="rc_gstom with comp=NH3 failed", thr=1.0e-5)
        if (allocated(error)) return

        dp_conf%has_vegetation = .true.
        meteo%glrad = 0.0
        call rc_gstom(comp, lu, meteo, dp_conf, dp_out, dp_err)
        call check(error, dp_out%gstom, 0.0, message="rc_gstom with glrad 0 failed", thr=1.0e-5)
        if (allocated(error)) return

    end subroutine test_rc_gstom

    subroutine test_get_vpd(error)
        type(error_type), allocatable, intent(out) :: error

        real :: vpd
        type(depac_meteorology) :: meteo

        call set_log_level(LOG_LEVEL_NONE)

        ! Test case 1: Normal conditions
        meteo%t = 20.0
        meteo%rh = 50.0
        call rc_get_vpd(meteo, vpd)
        call check(error, vpd,  1.16841877, message="get_vpd normal conditions failed", thr=1.0e-5)
        if (allocated(error)) return

        ! Test case 2: High humidity different temperature
        meteo%t = 25.0
        meteo%rh = 90.0
        call rc_get_vpd(meteo, vpd)
        call check(error, vpd, 0.316623241, message="get_vpd high humidity failed", thr=1.0e-5)
        if (allocated(error)) return

    end subroutine test_get_vpd

    subroutine test_par_dir_diff(error)
        type(error_type), allocatable, intent(out) :: error

        real :: par_diff, par_dir
        type(depac_meteorology) :: meteo

        call set_log_level(LOG_LEVEL_NONE)

        meteo%pres_0 = 1013.25
        meteo%sinphi = 0.5
        meteo%glrad = 500.0
        ! Test some realistinc sinphi values

        call par_dir_diff(meteo, par_dir, par_diff)
        call check(error, par_dir, 106.727577, message="par_dir_diff par_dir failed", thr=1.0e-4)
        if (allocated(error)) return
        call check(error, par_diff, 112.688774, message="par_dir_diff par_diff failed", thr=1.0e-4)
        if (allocated(error)) return

    end subroutine test_par_dir_diff

    subroutine test_rc_gstom_emb(error)
        type(error_type), allocatable, intent(out) :: error

        type(depac_land_use) :: lu
        type(depac_meteorology) :: meteo
        type(depac_config) :: dp_conf
        type(depac_output) :: dp_out
        real :: vpd
        real :: gstom


        call set_log_level(LOG_LEVEL_NONE)
        ! Add tests for rc_gstom embedded here

        lu%stom_par%F_min   = 0.01
        lu%stom_par%alpha   = 0.009
        lu%stom_par%Topt    = 26.0
        lu%stom_par%Tmin    = 12.0
        lu%stom_par%Tmax    = 40.0
        lu%stom_par%g_max   = 300.0
        lu%stom_par%vpd_max = 0.9
        lu%stom_par%vpd_min = 2.8


        ! no vegetation
        dp_conf%has_vegetation = .false.
        gstom = rc_gstom_emb(lu, meteo, vpd, dp_conf)
        call check(error, gstom, 0.0, &
            message="rc_gstom_emb no vegetation failed", thr=1.0e-5)
        if (allocated(error)) return

        ! with vegetation

        meteo%sinphi = 0.0
        meteo%glrad = 500.0
        meteo%pres_0 = 1013.25
        meteo%t = 25.0

        dp_conf%lai = 2.6

        vpd = 1.0

        !check if sinphi at 0 causes sinphi to be set to 0.0001 internally

        ! also lai > 2.5 and glrad > 200.
        dp_conf%has_vegetation = .true.
        gstom = rc_gstom_emb(lu, meteo, vpd, dp_conf)

        call check(error, meteo%sinphi, 0.0001, &
            message="rc_gstom_emb did not adjust sinphi correctly", thr=1.0e-5)
        if (allocated(error)) return

        call check(error, gstom, 7.35585546, &
            message="rc_gstom_emb with vegetation failed", thr=1.0e-5)
        if (allocated(error)) return

        meteo%sinphi = 0.3
        meteo%glrad = 150.0
        dp_conf%lai = 2.0

        gstom = rc_gstom_emb(lu, meteo, vpd, dp_conf)
        call check(error, gstom, 128.818481, &
            message="rc_gstom_emb with low glrad and lai failed", thr=1.0e-5)
        if (allocated(error)) return





    end subroutine test_rc_gstom_emb

end module m_test_rc_gstom