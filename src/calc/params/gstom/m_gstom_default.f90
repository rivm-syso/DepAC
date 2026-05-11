module m_gstom_default
    use c_depac_params, only: depac_gstom_param
    use t_depac_setup, only: depac_setup
    use t_depac_context, only: depac_context
    implicit none (type, external)
    private
    public :: gstom_default

    type, extends(depac_gstom_param) :: gstom_default
    contains
        procedure :: apply => gstom_default_apply
    end type gstom_default

contains
 pure function gstom_default_apply(this, setup, ctx) result(gstom)
      class(gstom_default), intent(in) :: this
      type(depac_setup), intent(in) :: setup
      type(depac_context), intent(in) :: ctx
      real :: gstom

      ! Default gstom parameterisation: returns 0.0 for all components and conditions
      ! This can be used as a placeholder or for components that do not require gstom.
      gstom = 0.0
 end function gstom_default_apply

end module m_gstom_default
