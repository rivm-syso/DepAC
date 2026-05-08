module m_rinc_default
   use t_depac_land_use, only: depac_rc_r_params
   use t_depac_meteorology, only: depac_meteorology
   use t_depac_config, only: depac_config
   implicit none (type, external)
   private
   public :: rinc_default
contains
   pure function rinc_default(rc_rinc, meteo, dp_conf) result(rinc)
      type(depac_rc_r_params), intent(in)  :: rc_rinc   ! current computed rinc parameters
      type(depac_meteorology), intent(in) :: meteo   ! current computed meteo
      type(depac_config), intent(in) :: dp_conf ! depac config

      real :: rinc

        if (meteo%ust > 0.0) then
            
            rinc = rc_rinc%b * rc_rinc%h * dp_conf%sai / meteo%ust
        else
            rinc = 1000.0
        endif
      

   end function rinc_default

end module m_rinc_default
