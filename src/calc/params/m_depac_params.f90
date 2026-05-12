!------------------------------------------------------------------------------
! Module:     m_depac_params
! Author:     Marte Voorneveld, RIVM
! Created:    May 12, 2026
! Modified:   May 12, 2026
! Description:
!   Central collection and re-export point for all parameterization
!   implementations used in the DepAC model. This module aggregates and
!   exposes gsoil, rinc, gstom, gw, compensation point, and rc_special
!   parameterizations, providing a single point of access for all model
!   parameters and their implementations.
!------------------------------------------------------------------------------

module m_depac_params
    ! Gsoil
    use m_gsoil_default, only: gsoil_default
    ! Rinc
    use m_rinc_default, only: rinc_default
    use m_rinc_no_path, only: rinc_no_path
    use m_rinc_no_resistance, only: rinc_no_resistance

    ! Gstom
    use m_gstom_default, only: gstom_default
    use m_gstom_emberson, only: gstom_emberson
    ! Gw
    use m_gw_default, only: gw_default
    use m_gw_so2, only: gw_so2
    use m_gw_nh3_sutton, only: gw_nh3_sutton

    ! Comp point
    use m_comp_point_ammonia, only: comp_point_ammonia
    use m_comp_point_default, only: comp_point_default

    ! Rc special
    use m_rc_tot_fixed, only: rc_tot_fixed
    use m_rc_tot_nitric_acid, only: rc_tot_nitric_acid
    use m_rc_tot_nitric_oxide, only: rc_tot_nitric_oxide
    use m_rc_special_default, only: rc_special_default

    ! Csoil
    use m_comp_point_ammonia, only: csoil_default, csoil_water



    implicit none (type, external)
    private
    public :: comp_point_ammonia, comp_point_default, csoil_default, csoil_water, &
       gsoil_default, rinc_default, rinc_no_path, rinc_no_resistance, &
       gstom_default, gstom_emberson, &
       gw_default, gw_so2, gw_nh3_sutton, &
       rc_tot_fixed, rc_tot_nitric_acid, rc_tot_nitric_oxide, rc_special_default
end module m_depac_params
