!------------------------------------------------------------------------------
! Module:     t_depac_output_core
! Author:     Marte Voorneveld, RIVM
! Created:    November 14, 2025
! Modified:   May 12, 2026
! Description:
!   Defines output type containing calculated DEPAC results (resistances,
!   conductances, compensation points, and version information). The
!   depac_output_core type stores all model output for a single calculation.
!------------------------------------------------------------------------------

module t_depac_output_core
    implicit none (type, external)
    public
    !> Type representing calculated DEPAC outputs.
    !! Contains the following fields:
    !! - gw: Leaf conductance (m/s, calculated, default -999.0).
    !! - gw_can: Canopy conductance (m/s, calculated, default -999.0).
    !! - gstom: Stomatal conductance (m/s, calculated, default -999.0).
    !! - ccomp_tot: Total compensation point (ug/m3, calculated, default -999.0).
    !! - gc_tot: Total canopy conductance (m/s, calculated, default -999.0).
    !! - gsoil_eff: Effective soil conductance (calculated, default -999.0).
    !! - rc_tot: Total canopy resistance (s/m, calculated, default -999.0).
    !! - rc_eff: Effective canopy resistance (s/m, calculated, default -999.0).
    !! - version: Version of the DepAC model.
    !! - build_date: Build date of the DepAC model.
    !! Note: The default values (-999.0) indicate missing or undefined data.
    type :: depac_output_core
      real :: gw = -999.0
      real :: gw_can = -999.0
      real :: gstom = -999.0
      real :: ccomp_tot = -999.0
      real :: gc_tot = -999.0
      real :: gsoil_eff = -999.0
      real :: rc_tot = -999.0
      real :: rc_eff = -999.0
      character(len=20) :: version = ""
      character(len=20) :: build_date = ""
    end type depac_output_core
end module t_depac_output_core