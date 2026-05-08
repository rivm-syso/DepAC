module m_rinc_no_path
   use t_depac_land_use, only: depac_rc_r_params, t_rinc_parameterisation
   use t_depac_meteorology, only: depac_meteorology
   use t_depac_config, only: depac_config
   implicit none (type, external)
   private
   public :: rinc_no_path

   type, extends(t_rinc_parameterisation) :: rinc_no_path
   contains
      procedure :: apply => rinc_no_path_apply
   end type rinc_no_path
contains
   pure function rinc_no_path_apply(this, rc_rinc, meteo, dp_conf) result(rinc)
      class(rinc_no_path), intent(in) :: this
      type(depac_rc_r_params), intent(in)  :: rc_rinc   ! current computed rinc parameters
      type(depac_meteorology), intent(in) :: meteo   ! current computed meteo
      type(depac_config), intent(in) :: dp_conf ! depac config

      real :: rinc

      rinc = -999.0

   end function rinc_no_path_apply

end module m_rinc_no_path
