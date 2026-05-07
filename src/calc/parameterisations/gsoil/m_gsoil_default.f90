module m_gsoil_default
   use t_depac_land_use, only: depac_land_use
   use t_depac_meteorology, only: depac_meteorology
   use t_depac_config, only: depac_config
   use m_helpers, only: missing
   implicit none (type, external)
   private
   public :: gsoil_default
contains
   pure function gsoil_default(lu_conf, meteo, dp_conf) result(gsoil)
      type(depac_land_use), intent(in)  :: lu_conf   ! current computed land-use type
      type(depac_meteorology), intent(in) :: meteo   ! current computed meteo
      type(depac_config), intent(in) :: dp_conf ! depac config
      real :: gsoil
      real :: rinc
      real :: rsoil_eff


      rinc = lu_conf%rc_rinc%rinc_param(lu_conf%rc_rinc, meteo, dp_conf)

      if(missing(rinc)) then
         gsoil = -9999.0
      else
         ! if (meteo%t < 0.0) then
         !    if (missing(lu_conf%rsoil_frozen)) then
         !       rsoil_eff = -9999.0
         !    else
         !       rsoil_eff = lu_conf%rsoil_frozen + rinc
         !    endif
         ! else
         !    if (meteo%nwet == 0) then
         !       if (missing(lu_conf%rsoil)) then
         !          rsoil_eff = -9999.0
         !       else
         !          rsoil_eff = lu_conf%rsoil + rinc
         !       endif
         !    elseif (meteo%nwet == 1) then
         !       if (missing(lu_conf%rsoil_wet)) then
         !          rsoil_eff = -9999.0
         !       else
         !          rsoil_eff = lu_conf%rsoil_wet + rinc
         !       endif
         !    else
         !       rsoil_eff = -9999.0 ! missing nwet value for wet soil condition
         !    endif
         ! endif
      endif

      ! Compute conductance:
        if (rsoil_eff > 0.0) then
            gsoil = 1.0/rsoil_eff
        else
            ! No deposition path, or missing value:
            gsoil = 0.0
        endif
   end function gsoil_default

end module m_gsoil_default
