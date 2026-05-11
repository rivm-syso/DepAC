!------------------------------------------------------------------------------
! Module:     m_rc_snow
! Authors:    Marte Voorneveld, RIVM
!             Addo van Pul, Jan Willem Erisman,
!             Ferd Sauter, Margreet van Zanten, Roy Wichink Kruit
! Created:    November 14 2025
! Updated:    November 20 2025
! Description:
!   This module provides routines for calculating snow resistance (rc_snow)
!   for atmospheric deposition modeling. The rc_snow subroutine computes
!   total resistance over snow surfaces using either a constant or
!   temperature-dependent parameterization, based on component and
!   meteorological input. Results are used in dry deposition calculations
!   and model output.
!------------------------------------------------------------------------------
module m_rc_snow
   use t_depac_context, only: depac_context
   use t_depac_setup, only: depac_setup

   use t_depac_error_core, only: ERR_INPUT
   use m_depac_error, only: set_error
   use m_logger, only: log_error

   implicit none (type, external)
   private
   public :: rc_snow
contains
   !------------------------------------------------------------------------------
   ! Subroutine: rc_snow
   ! Purpose:    Calculates total resistance over snow surfaces (rc_tot) for
   !             atmospheric deposition modeling, using either a constant or
   !             temperature-dependent parameterization.
   ! Arguments:
   !   - meteo   : Meteorological input (type depac_meteorology)
   !   - comp    : Component-specific parameters (type depac_component)
   !   - dp_conf : Model configuration (type depac_config)
   !   - dp_out  : Output structure, rc_tot is set here (type depac_output)
   !   - err     : Error handling structure (type depac_error)
   ! Notes:
   !   - Used in dry deposition calculations and model output.
   !------------------------------------------------------------------------------
   subroutine rc_snow(setup, ctx)
      type(depac_setup), intent(in) :: setup       ! setup
      type(depac_context), intent(inout) :: ctx    ! context
      associate(t => ctx%meteo%t, rc_tot => ctx%output%rc_tot, &
         ipar_snow => setup%component%ipar_snow)
         ! Choose parameterisation with constant or temperature dependent parameterisation:

         select case (ipar_snow)
          case (1)
            rc_tot = setup%config%rssnow
          case (2)
            if (t < -1.) then
               rc_tot = 500.
            elseif (t >  1.) then
               rc_tot = 70.
            else
               rc_tot = 70.*(2.-t)
            endif
          case default
            call set_error(ctx%error, ERR_INPUT, &
               'Programming error in rc_snow: unknown value of ipar_snow: '&
               //trim(setup%component%name))

            call log_error('Programming error in rc_snow: unknown value of ipar_snow: ' &
               //trim(setup%component%name))
            return
         end select
      end associate
   end subroutine rc_snow
end module m_rc_snow
