!------------------------------------------------------------------------------
! Module:     c_depac_param_types
! Author:     Marte Voorneveld, RIVM
! Created:    May 12, 2026
! Modified:   May 12, 2026
! Description:
!   Collector module for all parameterisation types to simplify imports
!   without requiring full component and land use types. Exposes parameterisation
!   abstract base types for all DEPAC resistance and conductance calculations.
!------------------------------------------------------------------------------

! this is just a collector module to make it easier to import all parameterisation types in one go, without having to import the full component and land use types which contain a lot of other fields that are not relevant for parameterisations.
module c_depac_param_types
    use t_depac_comp_point_param, only: depac_comp_point_param
    use t_depac_csoil_param, only: depac_csoil_param
    use t_depac_gsoil_param, only: depac_gsoil_param
    use t_depac_gstom_param, only: depac_gstom_param
    use t_depac_gw_param, only: depac_gw_param
    use t_depac_rc_special_param, only: depac_rc_special_param
    use t_depac_rinc_param, only: depac_rinc_param

    implicit none (type, external)
    private
    public :: depac_comp_point_param, depac_csoil_param, depac_gsoil_param, &
              depac_gstom_param, depac_gw_param, depac_rc_special_param, &
              depac_rinc_param
end module c_depac_param_types