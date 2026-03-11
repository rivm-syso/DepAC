!------------------------------------------------------------------------------
! Module:     m_logger
! Author:     Marte Voorneveld, RIVM
! Created:    November 14 2025
! Updated:    November 18 2025
! Description:
!   This module provides logging routines for the DepAC atmospheric deposition
!   model, supporting configurable log levels and message output for error,
!   warning, info, and debug events.
!------------------------------------------------------------------------------

module m_logger
    implicit none (type, external)
    public
    integer, parameter :: LOG_LEVEL_ERROR=1, LOG_LEVEL_WARN=2, LOG_LEVEL_INFO=3, LOG_LEVEL_DEBUG=4
    integer, parameter :: LOG_LEVEL_NONE=0
    integer :: log_level = LOG_LEVEL_WARN

contains

    subroutine set_log_level(level)
        integer, intent(in) :: level
        log_level = level
    end subroutine set_log_level

    subroutine log_error(msg)
        character(len=*), intent(in) :: msg
        if (log_level >= LOG_LEVEL_ERROR) print *, '[ERROR] ', msg
    end subroutine log_error

    subroutine log_warn(msg)
        character(len=*), intent(in) :: msg
        if (log_level >= LOG_LEVEL_WARN) print *, '[WARN] ', msg
    end subroutine log_warn

    subroutine log_info(msg)
        character(len=*), intent(in) :: msg
        if (log_level >= LOG_LEVEL_INFO) print *, '[INFO] ', msg
    end subroutine log_info

    subroutine log_debug(msg)
        character(len=*), intent(in) :: msg
        if (log_level >= LOG_LEVEL_DEBUG) print *, '[DEBUG] ', msg
    end subroutine log_debug

end module m_logger