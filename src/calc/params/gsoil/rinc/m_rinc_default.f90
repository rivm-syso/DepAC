module m_rinc_default
   use c_depac_params, only: depac_rinc_param
   use t_depac_setup, only: depac_setup
   use t_depac_context, only: depac_context
   implicit none (type, external)
   private
   public :: rinc_default

   type, extends(depac_rinc_param) :: rinc_default
   contains
      procedure :: apply => rinc_default_apply
   end type rinc_default
contains
   pure function rinc_default_apply(this, setup, ctx) result(rinc)
      class(rinc_default), intent(in) :: this
      type(depac_setup), intent(in) :: setup
      type(depac_context), intent(in) :: ctx

      real :: rinc
     
      associate(meteo => ctx%meteo, dp_conf => setup%config, rc_rinc => setup%land_use%rc_rinc)
        if (meteo%ust > 0.0) then

            rinc = rc_rinc%b * rc_rinc%h * dp_conf%sai / meteo%ust
        else
            rinc = 1000.0
        endif
      end associate
   end function rinc_default_apply

end module m_rinc_default
