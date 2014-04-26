#
#
#

AC_INIT([ardev], 
        [1.0], 
        [jorgefranconunes@gmail.com])

AM_INIT_AUTOMAKE([
        -Wall
        -Werror
        foreign
        subdir-objects
])

# Hack to make it work in automake above and below 1.11.
m4_ifdef([AM_PROG_AR], [AM_PROG_AR])

AC_PROG_CC
AC_PROG_CXX
AC_PROG_RANLIB

AC_CONFIG_HEADERS([config.h])

AC_CONFIG_FILES([
        Makefile
        ardev/Makefile
        demos/Makefile
])





#
# Define the location of the CppUTest library.
#
AC_MSG_CHECKING([for CppUTest])
AC_ARG_WITH([cpputest],
            [AS_HELP_STRING([--with-cpputest],
                            [support fancy command line editing @<:@default=check@:>@])],
            [],
            [with_cpputest=check])
          
CPPUTEST=
AS_IF([test "x$with_cpputest" != xcheck],
      [CPPUTEST=$with_cpputest],
      [AC_MSG_FAILURE([--with-cpputest is missing.])])
AC_MSG_RESULT([$with_cpputest])
AC_SUBST(CPPUTEST)





AC_OUTPUT
