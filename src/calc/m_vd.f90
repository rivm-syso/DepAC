module m_vd
    use t_depac_context, only: depac_context
    implicit none (type, external)
    private
    public :: depac_calc_vd_tot, depac_calc_vd_eff
contains
    !> Calculate the total deposition velocity (vd_tot) for a given state.
    !! This function computes vd_tot based on the provided depac_context and other parameters.
    pure function depac_calc_vd_tot(ctx) result(vd)
        type(depac_context), intent(in) :: ctx
        real :: vd
        real :: denom

        denom = ctx%state%ra_obs + ctx%state%rb + ctx%output%rc_tot

        if (abs(denom) > 1e-6) then
            vd = 1.0 / denom
        else
            vd = -999.0  ! Indicate invalid vd if denominator is too small
        end if
    end function depac_calc_vd_tot

    pure function depac_calc_vd_eff(ctx) result(vd_eff)
        type(depac_context), intent(in) :: ctx
        real :: vd_eff
        real :: denom

        denom = ctx%state%ra_obs + ctx%state%rb + ctx%output%rc_eff

        if (abs(denom) > 1e-6) then
            vd_eff = 1.0 / denom
        else
            vd_eff = -999.0  ! Indicate invalid vd_eff if denominator is too small
        end if
    end function depac_calc_vd_eff
end module m_vd
