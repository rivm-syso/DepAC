program basic_depac
    use depac, only: depac_calc, depac_component, depac_land_use, &
                     depac_meteorology, depac_config, depac_output, depac_error, has_error, &
                     VERSION, BUILD_DATE, COMP_NH3, LU_GRASS, VERSION, BUILD_DATE, obtain_config



    implicit none (type, external)

    type(depac_component) :: dp_comp
    type(depac_land_use) :: dp_lu
    type(depac_meteorology) :: dp_meteo
    type(depac_config) :: dp_conf
    type(depac_output) :: dp_out
    type(depac_error) :: dp_err



    print *, "Basic DepAC example program"
    print *, "DepAC version:", VERSION
    print *, "DepAC build date:", BUILD_DATE

    ! Initialize component and land use with example values
    call obtain_config(LU_GRASS, COMP_NH3, dp_comp, dp_lu, dp_err)

    if (has_error(dp_err)) then
        print *, "Error obtaining configuration:", dp_err%message
        stop 1
    end if

    ! Set configuration parameters for this example
    dp_conf%lai = 3.0
    dp_conf%sai = 3.5
    dp_conf%calc_comp_points = .true.
    dp_conf%calc_effective_rc = .true.
    dp_conf%ra_obs = 100.0
    dp_conf%rb = 50.0


    ! Set meteorological conditions for this example

    dp_meteo%t = 15.0
    dp_meteo%tsurf = 15.0
    dp_meteo%rh = 60.0
    dp_meteo%nwet = 0
    dp_meteo%glrad = 200.0
    dp_meteo%sinphi = 0.5
    dp_meteo%pres_0 = 101500.0

    ! set compenstation point for this component (example value)

    dp_conf%comp_point%c_ave_nh3 = 1.0      ! Average concentration in ug/m³
                                            ! (analogous to concentration inside leafs)
    dp_conf%comp_point%c_ave_so2 = 1.0      ! Average concentration in ug/m³
                                            ! (analogous to concentration inside leafs)
    dp_conf%comp_point%c_so2 = 1.2          ! Concentration in ug/m³
    dp_conf%comp_point%c_nh3 = 4.0          ! Concentration in ug/m³
    dp_conf%comp_point%iratns = 3


    call depac_calc(dp_comp, dp_lu, dp_meteo, dp_conf, dp_out, dp_err)
    if (has_error(dp_err)) then
        print *, "Error during DepAC calculation:", dp_err%message
        stop 1
    end if


    print *, "DepAC output:"
    print *, "Total canopy resistance (s/m):", dp_out%rc_tot
    print *, "Total canopy conductance (cm/s):", dp_out%gc_tot
    print *, "Compensation point (ug/m³):", dp_out%ccomp_tot

    print *, "Effective canopy resistance (s/m):", dp_out%rc_eff



end program basic_depac