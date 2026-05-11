module m_test_rc_gsoil
   use testdrive, only : new_unittest, unittest_type, error_type, check


   use m_test_gsoil_param, only: collect_gsoil_param_tests

   implicit none (type, external)
   private
   public :: collect_rc_gsoil_tests
contains

   subroutine collect_rc_gsoil_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)
      call collect_gsoil_param_tests(testsuite)

      testsuite = [testsuite, &
         new_unittest("rc_gsoil", test_rc_gsoil) &
      ]

   end subroutine collect_rc_gsoil_tests

   subroutine test_rc_gsoil(error)
      type(error_type), allocatable, intent(out) :: error




   end subroutine test_rc_gsoil

end module m_test_rc_gsoil
