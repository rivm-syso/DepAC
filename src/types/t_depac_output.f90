!------------------------------------------------------------------------------
! Module:     t_depac_output
! Author:     Marte Voorneveld, RIVM
! Created:    2025-11-14
! Updated:    2026-02-27
! Description:
!   This module defines the output type for the DepAC atmospheric deposition
!   model. All values in depac_output are calculated results, including
!   resistances, conductances, and compensation points.
!------------------------------------------------------------------------------
module t_depac_output
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
    type :: depac_output
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
    end type depac_output
end module t_depac_output