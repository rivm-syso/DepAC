!------------------------------------------------------------------------------
! Module:     m_rc_gw
! Authors:    Marte Voorneveld, RIVM
!             Addo van Pul, Jan Willem Erisman,
!             Ferd Sauter, Margreet van Zanten, Roy Wichink Kruit
! Created:    November 14 2025
! Updated:    November 20 2025
! Description:
!   Calculation of the external leaf conductance (gw)
!------------------------------------------------------------------------------
module m_rc_gw
   use t_depac_setup, only: depac_setup
   use t_depac_context, only: depac_context


   implicit none (type, external)
   public
contains

   subroutine rc_gw(setup, ctx)
      type(depac_setup), intent(in) :: setup
      type(depac_context), intent(inout) :: ctx


      ctx%output%gw = setup%gw_param%apply(setup, ctx)
   end subroutine rc_gw
end module m_rc_gw
