module m_test_comp_points
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use t_depac_component, only: depac_component
    use t_depac_land_use, only: depac_land_use
    use t_depac_meteorology, only: depac_meteorology
    use t_depac_config, only: depac_config
    use t_depac_output, only: depac_output
    use t_depac_error, only: depac_error, ERR_INPUT
    use m_logger, only: set_log_level, LOG_LEVEL_NONE
    use m_helpers, only: missing
    use m_depac_error, only: set_error, has_error
    use m_comp_points, only: rc_comp_point
    use default_indices, only: LU_GRASS, LU_OTHER, COMP_HNO3, &
     COMP_NH3, COMP_O3, COMP_SO2, COMP_NO2, COMP_NO, LU_WATER

    implicit none (type, external)
    private
    public :: collect_comp_points_tests
    contains

    subroutine collect_comp_points_tests(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("rc_comp_point", test_rc_comp_point) &
        ]

    end subroutine collect_comp_points_tests

    subroutine test_rc_comp_point(error)
        type(error_type), allocatable, intent(out) :: error

        type(depac_component) :: comp
        type(depac_land_use) :: lu
        type(depac_meteorology) :: meteo
        type(depac_config) :: dp_conf
        type(depac_output) :: dp_out
        type(depac_error) :: dp_err
        logical :: ready

        call set_log_level(LOG_LEVEL_NONE)

        ! Add test cases for rc_comp_point here


        ! test non-important components
        comp%name = "SO2"
        comp%index = COMP_SO2
        call rc_comp_point(comp, lu, meteo, dp_conf, dp_out, dp_err)
        call check(error, dp_out%ccomp_tot, 0.0, message="rc_comp_point SO2 failed", thr=1.0e-5)
        if (allocated(error)) return

        comp%name = "NO2"
        comp%index = COMP_NO2
        call rc_comp_point(comp, lu, meteo, dp_conf, dp_out, dp_err)
        call check(error, dp_out%ccomp_tot, 0.0, message="rc_comp_point NO2 failed", thr=1.0e-5)
        if (allocated(error)) return

        comp%name = "NO"
        comp%index = COMP_NO
        call rc_comp_point(comp, lu, meteo, dp_conf, dp_out, dp_err)
        call check(error, dp_out%ccomp_tot, 0.0, message="rc_comp_point NO failed", thr=1.0e-5)
        if (allocated(error)) return

        comp%name = "O3"
            comp%index = COMP_O3
        call rc_comp_point(comp, lu, meteo, dp_conf, dp_out, dp_err)
        call check(error, dp_out%ccomp_tot, 0.0, message="rc_comp_point O3 failed", thr=1.0e-5)
        if (allocated(error)) return

        comp%name = "NH3"
        comp%index = COMP_NH3
        lu%name = "grass"
        lu%index = LU_GRASS
        meteo%tsurf = 21.0
        meteo%t = 21.0
        dp_conf%has_leaves = .true.
        dp_conf%has_vegetation = .true.
        dp_conf%comp_point%c_ave_nh3 = 0.7
        dp_conf%comp_point%c_nh3 = 6.0
        dp_conf%comp_point%c_ave_so2 = 0.9
        lu%gamma_stom_c_fac = 0.1
        lu%gamma_soil_c_fac = 0.1
        dp_out%gc_tot = 0.0009
        dp_out%gw = 1.0
        dp_out%gstom = 1.0
        dp_out%gsoil_eff = 2.0

        call rc_comp_point(comp, lu, meteo, dp_conf, dp_out, dp_err)
        call check(error, dp_out%ccomp_tot, 755.032166,&
             message="rc_comp_point NH3 failed", thr=1.0e-2)
        if (allocated(error)) return

        lu%gamma_soil_c_fac = -0.5

        call rc_comp_point(comp, lu, meteo, dp_conf, dp_out, dp_err)
        call check(error, dp_out%ccomp_tot, 755.032166,&
             message="rc_comp_point NH3 negative gamma_soil_c_fac failed", thr=1.0e-5)
        if (allocated(error)) return

        lu%name = "water"
        lu%index = LU_WATER
        ! test Water
        call rc_comp_point(comp, lu, meteo, dp_conf, dp_out, dp_err)
        call check(error, dp_out%ccomp_tot, 758.243286,&
             message="rc_comp_point water failed", thr=1.0e-5)
        if (allocated(error)) return
        ! test positive gamma_soil_c_fac for water
        lu%gamma_soil_c_fac = 0.5
        call rc_comp_point(comp, lu, meteo, dp_conf, dp_out, dp_err)
        call check(error, dp_out%ccomp_tot, 759.619446,&
             message="rc_comp_point water positive gamma_soil_c_fac failed", thr=1.0e-5)
        if (allocated(error)) return

        lu%name = "grass"
        lu%index = LU_GRASS
        lu%gamma_soil_c_fac = -0.5


        ! test negative c_ave_nh3
        dp_conf%comp_point%c_ave_nh3 = -0.1

        call rc_comp_point(comp, lu, meteo, dp_conf, dp_out, dp_err)
        call check(error, dp_out%ccomp_tot, 1127.75928,&
             message="rc_comp_point NH3 negative c_ave_nh3 failed", thr=1.0e-5)
        if (allocated(error)) return

        lu%gamma_stom_c_fac = 0.1
        dp_conf%comp_point%c_ave_nh3 = 12
        dp_conf%comp_point%c_ave_so2 = -0.1

        call rc_comp_point(comp, lu, meteo, dp_conf, dp_out, dp_err)
        call check(error, dp_out%ccomp_tot, 1133.58435,&
             message="rc_comp_point NH3 negative c_ave_so2 failed", thr=1.0e-5)
        if (allocated(error)) return

        dp_conf%has_vegetation = .false.

        call rc_comp_point(comp, lu, meteo, dp_conf, dp_out, dp_err)
        call check(error, dp_out%ccomp_tot, 5.82508612,&
             message="rc_comp_point NH3 no vegetation failed", thr=1.0e-5)
        if (allocated(error)) return


          ! Test negative gc_tot
        dp_out%gc_tot = -0.1
        call rc_comp_point(comp, lu, meteo, dp_conf, dp_out, dp_err)
        call check(error, dp_out%ccomp_tot, 0.0,&
             message="rc_comp_point NH3 negative gc_tot failed", thr=1.0e-5)
        if (allocated(error)) return

        ! Throw an error and see if allocated
        ! fake component
        comp%name = "K3"
        comp%index = 100
        call rc_comp_point(comp, lu, meteo, dp_conf, dp_out, dp_err)
        call check(error, has_error(dp_err), .true., &
            message="rc_comp_point invalid component failed")




    end subroutine test_rc_comp_point

end module m_test_comp_points