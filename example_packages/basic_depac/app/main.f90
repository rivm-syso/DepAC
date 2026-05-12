program basic_depac
    use depac, only: depac_calc,  depac_context, depac_setup, &
        has_error, VERSION, BUILD_DATE

    use default_depac_config_rivm, only: default_depac_setup, &
        init_default_depac_config_rivm, &
        finalize_default_depac_config_rivm, &
        RIVM_LU_GRASS, RIVM_COMP_NH3


    implicit none (type, external)
    type(depac_context) :: dp_ctx
    type(depac_setup) :: dp_setup


    print *, "Basic DepAC example program"
    print *, "DepAC version:", VERSION
    print *, "DepAC build date:", BUILD_DATE

    call init_default_depac_config_rivm()

    dp_setup = default_depac_setup(RIVM_LU_GRASS, RIVM_COMP_NH3)

    ! Set configuration parameters for this example
    dp_setup%config%lai = 3.0
    dp_setup%config%sai = 3.5
    dp_setup%config%calc_comp_points = .true.
    dp_setup%config%calc_effective_rc = .true.
    dp_setup%config%ra_obs = 100.0
    dp_setup%config%rb = 50.0


    ! Set meteorological conditions for this example

    dp_ctx%meteo%t = 15.0
    dp_ctx%meteo%tsurf = 15.0
    dp_ctx%meteo%rh = 60.0
    dp_ctx%meteo%nwet = 0
    dp_ctx%meteo%glrad = 200.0
    dp_ctx%meteo%sinphi = 0.5
    dp_ctx%meteo%pres_0 = 101500.0

    ! set compenstation point for this component (example value)

    dp_setup%config%comp_point%c_ave_nh3 = 1.0      ! Average concentration in ug/m³
                                            ! (analogous to concentration inside leafs)
    dp_setup%config%comp_point%c_ave_so2 = 1.0      ! Average concentration in ug/m³
                                            ! (analogous to concentration inside leafs)
    dp_setup%config%comp_point%c_so2 = 1.2          ! Concentration in ug/m³
    dp_setup%config%comp_point%c_nh3 = 4.0          ! Concentration in ug/m³
    dp_setup%config%comp_point%iratns = 3


    call depac_calc(dp_setup, dp_ctx)

    if (has_error(dp_ctx%error)) then
        print *, "Error during DepAC calculation:", dp_ctx%error%message
        stop 1
    end if


    print *, "DepAC output:"
    print *, "Total canopy resistance (s/m):", dp_ctx%output%rc_tot
    print *, "Total canopy conductance (cm/s):", dp_ctx%output%gc_tot
    print *, "Compensation point (ug/m³):", dp_ctx%output%ccomp_tot

    print *, "Effective canopy resistance (s/m):", dp_ctx%output%rc_eff

end program basic_depac