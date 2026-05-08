module m_rc_tot_nitric_acid
   use t_depac_meteorology, only: depac_meteorology
   use t_depac_output, only: depac_output
   use t_depac_error, only: depac_error
   use t_depac_component, only: t_rc_special
   use t_depac_config, only: depac_config
   use t_depac_component_core, only: depac_component_core
   ! nitric acid parameterisation
   ! HNO3 is treated as a special case, with only one total canopy resistance
   implicit none (type, external)
   private
   public :: t_rc_tot_nitric_acid

   type, extends(t_rc_special) :: t_rc_tot_nitric_acid
   contains
      procedure :: apply => rc_tot_nitric_acid
   end type t_rc_tot_nitric_acid

contains

   subroutine rc_tot_nitric_acid(this, meteo, comp, dp_conf, dp_out, err, ready)
      class(t_rc_tot_nitric_acid), intent(in) :: this
      type(depac_meteorology), intent(in) :: meteo
      class(depac_component_core), intent(in) :: comp
      type(depac_config), intent(in) :: dp_conf
      type(depac_output), intent(inout) :: dp_out
      type(depac_error), intent(inout) :: err
      logical, intent(inout) :: ready

      if (meteo%t < -5.0 .and. meteo%nwet == 9) then
         ! T < 5 C and snow:
         dp_out%rc_tot = 50.
      else
         ! all other circumstances:
         dp_out%rc_tot = 10.0
      endif
      ready = .true.
   end subroutine rc_tot_nitric_acid
end module m_rc_tot_nitric_acid
