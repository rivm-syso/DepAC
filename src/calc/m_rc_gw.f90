!------------------------------------------------------------------------------
! Module:     m_rc_gw
! Authors:    Marte Voorneveld, RIVM
!             Addo van Pul, Jan Willem Erisman,
!             Ferd Sauter, Margreet van Zanten, Roy Wichink Kruit
! Created:    November 14 2025
! Updated:    November 20 2025
! Description:
!   This module provides routines for calculating external leaf resistance
!   (gw) for atmospheric deposition modeling components. The main subroutine,
!   rc_gw, computes conductance for NO, NO2, O3, SO2, and NH3, using
!   meteorological, component, and configuration parameters. Includes
!   parameterizations for SO2 and NH3 based on literature and RIVM defaults.
!------------------------------------------------------------------------------
module m_rc_gw
   use t_depac_component, only: depac_component
   use t_depac_config, only: depac_config
   use t_depac_meteorology, only: depac_meteorology
   use t_depac_output, only: depac_output
   use m_logger, only: log_info, log_warn, log_error
   use m_depac_error, only: set_error
   use t_depac_error, only: ERR_INPUT, depac_error
   use m_helpers, only: missing

   implicit none (type, external)
   public
contains

   subroutine rc_gw(comp, meteo, dp_conf, dp_out, err)
      type(depac_component), intent(in) :: comp      ! current computed component
      type(depac_config), intent(in) :: dp_conf ! depac config
      type(depac_meteorology), intent(in) :: meteo     ! current computed meteo
      type(depac_output), intent(inout) :: dp_out
      type(depac_error), intent(inout) :: err

      ! calculate the external leaf resistance (gw) for the current component and meteorological conditions
      ! we use the parameterisation comp%gw_param
      if(.not. associated(comp%gw_param)) then
         call set_error(err, ERR_INPUT, 'gw_param not associated for component '//trim(comp%name))
         call log_error('gw_param not associated for component '//trim(comp%name))
         return
      endif

      dp_out%gw = comp%gw_param(meteo, comp, dp_conf, err)

   end subroutine rc_gw




end module m_rc_gw
