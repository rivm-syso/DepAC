module m_test_gsoil_param
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use t_depac_land_use, only: depac_land_use, depac_rc_r_params, &
        t_gsoil_parameterisation, t_rinc_parameterisation
    use t_depac_meteorology, only: depac_meteorology
    use t_depac_config, only: depac_config
    use t_depac_component, only: depac_component

    use m_gsoil_param, only: gsoil_default, rinc_default, rinc_no_path, rinc_no_resistance
    implicit none (type, external)
    private
    public :: collect_gsoil_param_tests
contains
    subroutine collect_gsoil_param_tests(testsuite)
        type(unittest_type), allocatable, intent(inout) :: testsuite(:)

        testsuite = [testsuite, &
            new_unittest("gsoil_default", test_gsoil_default), &
            new_unittest("rsoil_rinc_default", test_rsoil_rinc_default), &
            new_unittest("rsoil_rinc_no_path", test_rsoil_rinc_no_path), &
            new_unittest("rsoil_rinc_no_resistance", test_rsoil_rinc_no_resistance) &
        ]

    end subroutine collect_gsoil_param_tests

    subroutine test_gsoil_default(error)
        type(error_type), allocatable, intent(out) :: error
        type(depac_land_use) :: lu
        type(depac_component) :: comp
        type(depac_meteorology) :: meteo
        type(depac_config) :: dp_conf
        real :: gsoil
        class(t_gsoil_parameterisation), allocatable :: gsoil_f

        allocate(lu%rc_rinc%rinc_param, source=rinc_no_path())

        allocate(gsoil_f, source=gsoil_default())

        gsoil = gsoil_f%apply(lu, comp, meteo, dp_conf)


        call check(error, gsoil, 0.0, message="missing(rinc) failed", thr=1.0e-5)
        if (allocated(error)) return

        deallocate(lu%rc_rinc%rinc_param)
        allocate(lu%rc_rinc%rinc_param, source=rinc_no_resistance())
        meteo%t = -5.0 ! negative temperature
        comp%rsoil_frozen = 1.0
        gsoil = gsoil_f%apply(lu, comp, meteo, dp_conf)
        call check(error, gsoil, 1.0, &
        message="frozen soil with no rinc resistance failed", thr=1.0e-5)
        if (allocated(error)) return

        !missing rsoil_frozen
        comp%rsoil_frozen = -999.0
        gsoil = gsoil_f%apply(lu, comp, meteo, dp_conf)

        call check(error, gsoil, 0.0, &
            message="missing rsoil_frozen did not result in gsoil=0", thr=1.0e-5)
        if (allocated(error)) return

        meteo%t = 5.0 ! positive temperature
        meteo%nwet = 0 ! dry soil condition
        lu%rsoil = 2.0
        gsoil = gsoil_f%apply(lu, comp, meteo, dp_conf)
        call check(error, gsoil, 0.5, &
             message="dry soil with no rinc resistance failed", thr=1.0e-5)
        if (allocated(error)) return

        meteo%nwet = 1 ! wet soil condition
        comp%rsoil_wet = 3.0
        gsoil = gsoil_f%apply(lu, comp, meteo, dp_conf)


        call check(error, gsoil, 0.3333333333, &
            message="wet soil with no rinc resistance failed", thr=1.0e-5)
        if (allocated(error)) return

         ! invalid nwet value
        meteo%nwet = 2
        gsoil = gsoil_f%apply(lu, comp, meteo, dp_conf)
        call check(error, gsoil, 0.0, &
        message="invalid nwet value did not result in gsoil=0", thr=1.0e-5)
        if (allocated(error)) return

        meteo%nwet = 1 ! wet soil condition
        comp%rsoil_wet = -999.0 ! missing rsoil_wet
        gsoil = gsoil_f%apply(lu, comp, meteo, dp_conf)
        call check(error, gsoil, 0.0, &
            message="missing rsoil_wet did not result in gsoil=0", thr=1.0e-5)
        if (allocated(error)) return

        meteo%nwet = 0 ! dry soil condition
        lu%rsoil = -999.0 ! missing lu%rsoil
        gsoil = gsoil_f%apply(lu, comp, meteo, dp_conf)
        call check(error, gsoil, 0.0, &
            message="missing lu%rsoil did not result in gsoil=0", thr=1.0e-5)
        if (allocated(error)) return



        deallocate(gsoil_f)
    end subroutine test_gsoil_default

    subroutine test_rsoil_rinc_default(error)
        type(error_type), allocatable, intent(out) :: error
        type(depac_rc_r_params) :: rc_rinc
        type(depac_meteorology) :: meteo
        type(depac_config) :: dp_conf
        real :: rinc
        class(t_rinc_parameterisation), allocatable :: rinc_f

        allocate(rinc_f, source=rinc_default())

        meteo%ust = -1.0 ! negative ust to test missing rinc resistance

        rinc = rinc_f%apply(rc_rinc, meteo, dp_conf)
        call check(error, rinc, 1000.0, &
        message="rinc with negative ust did not result in rinc=1000.0", thr=1.0e-5)
        if (allocated(error)) return

        ! positive ust to test normal rinc calculation
        meteo%ust = 3.0

        rc_rinc%b  = 1.2
        rc_rinc%h  = 5.5
        dp_conf%sai = 6.0

        rinc = rc_rinc%b * rc_rinc%h * dp_conf%sai


        rinc = rinc_f%apply(rc_rinc, meteo, dp_conf)

        call check(error, rinc, 13.2, &
            message="rinc with positive ust did not calculate correctly", thr=1.0e-5)
        if (allocated(error)) return


        deallocate(rinc_f)
    end subroutine test_rsoil_rinc_default

    subroutine test_rsoil_rinc_no_path(error)
        type(error_type), allocatable, intent(out) :: error

        type(depac_rc_r_params) :: rc_rinc
        type(depac_meteorology) :: meteo
        type(depac_config) :: dp_conf
        real :: rinc
        class(t_rinc_parameterisation), allocatable :: rinc_f

        allocate(rinc_f, source=rinc_no_path())

        rinc = rinc_f%apply(rc_rinc, meteo, dp_conf)


        call check(error, rinc, -999.0, message="rinc_no_path did not return -999.0", thr=1.0e-5)
        if (allocated(error)) return

        deallocate(rinc_f)
    end subroutine test_rsoil_rinc_no_path

    subroutine test_rsoil_rinc_no_resistance(error)
        type(error_type), allocatable, intent(out) :: error

        type(depac_rc_r_params) :: rc_rinc
        type(depac_meteorology) :: meteo
        type(depac_config) :: dp_conf
        real :: rinc
        class(t_rinc_parameterisation), allocatable :: rinc_f

        allocate(rinc_f, source=rinc_no_resistance())
        rinc = rinc_f%apply(rc_rinc, meteo, dp_conf)

        call check(error, rinc, 0.0, message="rinc_no_resistance did not return 0.0", thr=1.0e-5)
        if (allocated(error)) return
        deallocate(rinc_f)

    end subroutine test_rsoil_rinc_no_resistance

end module m_test_gsoil_param