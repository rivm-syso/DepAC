program basic_depac
    use depac, only: depac_calc,  depac_context, depac_setup, &
        has_error, VERSION, BUILD_DATE

    use default_depac_config_rivm, only: default_depac_setup, &
        init_default_depac_config_rivm, &
        finalize_default_depac_config_rivm, &
        RIVM_LU_GRASS, RIVM_COMP_NH3

    ! Import the custom parameterisation module for external leaf conductance (gw)
    use m_gw_custom, only: gw_custom


    implicit none (type, external)

    ! The depac_context type holds all runtime state data. It is the data that dynamically changes
    ! within a certain setup

    type(depac_context) :: dp_ctx

    ! The depac_setup type holds all configuration and parameterisation data. It is the data that
    ! defines the setup for a certain land use and component combination.
    ! It includes dynamic classes for the configuration of custom parameterisations, 
    type(depac_setup) :: dp_setup


    print *, "Basic DepAC example program"
    print *, "DepAC version:", VERSION
    print *, "DepAC build date:", BUILD_DATE

    call init_default_depac_config_rivm()

    dp_setup = default_depac_setup(RIVM_LU_GRASS, RIVM_COMP_NH3)

    ! Set custom parameterisation for external leaf conductance (gw)
    deallocate(dp_setup%gw_param)  ! Deallocate default gw parameterisation
    allocate(dp_setup%gw_param, source=gw_custom()) ! use our custom parameterisation

    dp_setup%config%calc_comp_points = .true.
    dp_setup%config%calc_effective_rc = .true.

    ! Set configuration parameters for this example
    dp_ctx%state%lai = 3.0
    dp_ctx%state%sai = 3.5
    
    dp_ctx%state%ra_obs = 100.0
    dp_ctx%state%rb = 50.0


    ! ! Set meteorological conditions for this example

    dp_ctx%meteo%t = 15.0
    dp_ctx%meteo%tsurf = 15.0
    dp_ctx%meteo%rh = 60.0
    dp_ctx%meteo%nwet = 0
    dp_ctx%meteo%glrad = 200.0
    dp_ctx%meteo%sinphi = 0.5
    dp_ctx%meteo%pres_0 = 101500.0

    ! ! set compenstation point for this component (example value)

    dp_ctx%state%comp_point%c_ave_nh3 = 1.0      ! Average concentration in ug/m³
                                            ! (analogous to concentration inside leafs)
    dp_ctx%state%comp_point%c_ave_so2 = 1.0

    dp_ctx%state%comp_point%c_so2 = 1.2          ! Concentration in ug/m³
    dp_ctx%state%comp_point%c_nh3 = 4.0          ! Concentration in ug/m³
    dp_ctx%state%comp_point%iratns = 3


    call depac_calc(dp_setup, dp_ctx)

    if (has_error(dp_ctx%error)) then
        print *, "Error during DepAC calculation:", dp_ctx%error%message
        stop 1
    end if


    print *, "DepAC output:"
    print *, "Total canopy resistance (s/m):", dp_ctx%output%rc_tot
    print *, "Total canopy conductance (cm/s):", dp_ctx%output%gc_tot
    print *, "Compensation point (ug/m³):", dp_ctx%output%ccomp_tot

    print *, "Soil resistance (s/m):", dp_ctx%output%gw


    dp_ctx%meteo%t = -5.0 ! now frozen conditions, see how this affects the custom gw parameterisation

    call depac_calc(dp_setup, dp_ctx)

    if (has_error(dp_ctx%error)) then
        print *, "Error during DepAC calculation:", dp_ctx%error%message
        stop 1
    end if

    print *, "DepAC output under frozen conditions:"
    print *, "Total canopy resistance (s/m):", dp_ctx%output%rc_tot
    print *, "Total canopy conductance (cm/s):", dp_ctx%output%gc_tot
    print *, "Compensation point (ug/m³):", dp_ctx%output%ccomp_tot

    print *, "Soil resistance (s/m):", dp_ctx%output%gw

end program basic_depac