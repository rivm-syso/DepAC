!------------------------------------------------------------------------------
! Module:     m_comp_points
! Author:     Marte Voorneveld, Addo van Pul, Jan Willem Erisman,
!                       Ferd Sauter, Margreet van Zanten, Roy Wichink Kruit
! Created:    November 14 2025
! Updated:    November 20 2025
! Description:
!   This module provides routines for calculating compensation points for
!   atmospheric deposition modeling components, especially NH3. The main
!   subroutine, rc_comp_point, computes compensation points for stomatal,
!   external leaf, and soil pathways, using meteorological, land use, and
!   component-specific parameters. The results are used in dry deposition
!   calculations and model output.
!------------------------------------------------------------------------------
module m_comp_points
    use t_depac_component, only: depac_component
    use t_depac_land_use, only: depac_land_use
    use t_depac_meteorology, only: depac_meteorology
    use t_depac_output, only: depac_output
    use t_depac_config, only: depac_config
    use m_logger, only: log_error, log_info
    use m_depac_error, only: set_error
    use t_depac_error, only: ERR_INPUT, depac_error
    use default_indices, only: LU_WATER, COMP_NH3, COMP_NO, COMP_NO2, COMP_O3, COMP_SO2
    implicit none (type, external)
    private
    public :: rc_comp_point
contains
    !-------------------------------------------------------------------
    ! rc_comp_point: compute compensation points for NH3 and other components
    !-------------------------------------------------------------------
    subroutine rc_comp_point(comp, lu_conf, meteo, dp_conf, dp_out, err)
        type(depac_component), intent(in) :: comp      ! current computed component
        type(depac_land_use), intent(in)  :: lu_conf   ! current computed land-use type
        type(depac_meteorology), intent(in) :: meteo   ! current computed meteo
        type(depac_config), intent(in) :: dp_conf ! depac config
        type(depac_output), intent(inout) :: dp_out ! output of this run
        type(depac_error), intent(inout) :: err  ! error handling


        real :: cw, cstom, csoil, gamma_stom, gamma_soil, gamma_w, tk, tfac, co_dep_fac

        select case(comp%index)
        case(COMP_NO, COMP_NO2, COMP_O3, COMP_SO2)
            ! no compensation points:
            dp_out%ccomp_tot  = 0.0

        case(COMP_NH3)
            tk = meteo%tsurf + 273.15
            tfac = (2.75e15/tk)*exp(-1.04e4/tk)

            ! Stomatal compensation point:
            if (dp_conf%has_leaves .and. dp_conf%comp_point%c_ave_nh3 > 0.) then
                gamma_stom = lu_conf%gamma_stom_c_fac * dp_conf%comp_point%c_ave_nh3 * &
                     4.7 * exp(-0.071*meteo%t)

                cstom = max(0.0, gamma_stom*tfac)
            else
                cstom = 0.0
            endif

            ! External leaf compensation point:
            if (dp_conf%has_vegetation .and. &
                        dp_conf%comp_point%c_ave_nh3 > 0. .and. &
                        dp_conf%comp_point%c_ave_so2 > 0.) then

                gamma_w = -850. + 1840. * dp_conf%comp_point%c_nh3 * exp(-0.11*meteo%t)
                co_dep_fac = 1.12 - 1.32 * ((dp_conf%comp_point%c_ave_so2/64.) / &
                            (dp_conf%comp_point%c_ave_nh3/17.))
                co_dep_fac = max(0.0, co_dep_fac)
                gamma_w = co_dep_fac * gamma_w
                cw = max(0.0, gamma_w*tfac)
            elseif (dp_conf%has_vegetation) then
                gamma_w = -850. + 1840. * dp_conf%comp_point%c_nh3 * exp(-0.11*meteo%t)
                cw = max(0.0, gamma_w*tfac)
            else
                cw = 0.0
            endif

            ! Soil compensation point:
            if (lu_conf%index == LU_WATER) then
                if (lu_conf%gamma_soil_c_fac > 0) then
                    gamma_soil = lu_conf%gamma_soil_c_fac * 1.0
                else
                    gamma_soil = abs(lu_conf%gamma_soil_c_fac) * dp_conf%comp_point%c_ave_nh3
                endif
                csoil = gamma_soil * tfac
            else
                csoil = 0.0
            endif

            ! Total compensation point is weighed average of separate compensation points:
            if (dp_out%gc_tot > 0.0) then
                dp_out%ccomp_tot = (dp_out%gw/dp_out%gc_tot)*cw + &
                    (dp_out%gstom/dp_out%gc_tot)*cstom + &
                    (dp_out%gsoil_eff/dp_out%gc_tot)*csoil
            else
                dp_out%ccomp_tot = 0.0
            endif

        case default
            call set_error(err, ERR_INPUT, 'Component '//trim(comp%name) &
                        //' not supported in rc_comp_point')
            call log_error('Component '//trim(comp%name)//' not supported in rc_comp_point')
            return
        end select
    end subroutine rc_comp_point
end module m_comp_points