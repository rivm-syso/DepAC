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
    use t_depac_setup, only: depac_setup
    use t_depac_context, only: depac_context
    implicit none (type, external)
    private
    public :: rc_comp_point
contains
    !-------------------------------------------------------------------
    ! rc_comp_point: compute compensation points for NH3 and other components
    !-------------------------------------------------------------------
    subroutine rc_comp_point(setup, ctx)
        type(depac_setup), intent(in) :: setup
        type(depac_context), intent(inout)  :: ctx     ! current computed context


        ctx%output%ccomp_tot = setup%comp_point_param%apply(setup, ctx)
    end subroutine rc_comp_point
end module m_comp_points