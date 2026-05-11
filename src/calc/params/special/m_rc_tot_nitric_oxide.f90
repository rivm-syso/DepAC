module m_rc_tot_nitric_oxide
    use c_depac_param_types, only: depac_rc_special_param
    use t_depac_setup, only: depac_setup
    use t_depac_context, only: depac_context
    use m_rc_snow, only: rc_snow
    ! nitric oxide parameterisation
    ! NO is treated as a special case, with only one total canopy resistance
    implicit none (type, external)
    private
    public :: rc_tot_nitric_oxide

    type, extends(depac_rc_special_param) :: rc_tot_nitric_oxide
        real :: fixed_rc_tot = 2000.0
    contains
        procedure :: apply => rc_tot_nitric_oxide_apply
    end type rc_tot_nitric_oxide
contains
    subroutine rc_tot_nitric_oxide_apply(this, setup, ctx, ready)
        class(rc_tot_nitric_oxide), intent(in) :: this
        class(*), intent(in) :: setup
        type(depac_context), intent(inout) :: ctx
        logical, intent(inout) :: ready

        select type (setup)
         type is (depac_setup)
            if (ctx%meteo%nwet == 1) then
                ctx%output%rc_tot = this%fixed_rc_tot
                ready = .true.
            else if (ctx%meteo%nwet == 9) then
                ! snow surface:
                call rc_snow(setup, ctx)
                ready = .true.
            end if
         class default
            ! If the setup is not of type depac_setup, set ready to false and do not modify rc_tot
            ready = .false.
      end select

    end subroutine rc_tot_nitric_oxide_apply


end module m_rc_tot_nitric_oxide