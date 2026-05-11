!------------------------------------------------------------------------------
! Module:     m_depac_error
! Author:     Marte Voorneveld, RIVM
! Created:    November 14 2025
! Updated:    November 18 2025
! Description:
!   This module provides error handling routines for the DepAC atmospheric
!   deposition model, including setting, clearing, and checking error status.
!------------------------------------------------------------------------------

module m_depac_error
    use t_depac_error_core, only: depac_error_core, ERR_NONE
    implicit none (type, external)
    public

    contains

    subroutine set_error(err, code, message)
        type(depac_error_core), intent(inout) :: err
        integer, intent(in) :: code
        character(len=*), intent(in) :: message

        err%code = code
        err%message = message
    end subroutine set_error

    subroutine clear_error(err)
        type(depac_error_core), intent(inout) :: err

        err%code = ERR_NONE
        err%message = ''
    end subroutine clear_error

    function has_error(err) result(result)
        type(depac_error_core), intent(in) :: err
        logical :: result

        result = (err%code /= ERR_NONE)
    end function has_error

end module m_depac_error