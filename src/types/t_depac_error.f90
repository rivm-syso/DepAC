!------------------------------------------------------------------------------
! Module:     t_depac_error
! Author:     Marte Voorneveld, RIVM
! Created:    2025-11-14
! Updated:    2026-02-27
! Description:
!   This module defines error codes and the error type for the DepAC
!   atmospheric deposition model. It provides standardized error handling
!   for input validation, computation, memory, and unknown errors.
!------------------------------------------------------------------------------
module t_depac_error
    implicit none (type, external)
    public

    ! Define error codes
    integer, parameter :: ERR_NONE = 0
    integer, parameter :: ERR_INPUT = 1
    integer, parameter :: ERR_COMPUTATION = 2
    integer, parameter :: ERR_MEMORY = 3
    integer, parameter :: ERR_UNKNOWN = 4

    !> Type representing a DEPAC error state.
    !! Contains the following fields:
    !! - code: Error code (see ERR_* parameters).
    !! - message: Human-readable error message.
    type :: depac_error
        integer :: code
        character(len=256) :: message
    end type depac_error
end module t_depac_error