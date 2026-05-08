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


        dp_out%ccomp_tot = comp%comp_point_param%apply(meteo, lu_conf, dp_conf, dp_out)

    end subroutine rc_comp_point
end module m_comp_points