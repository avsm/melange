AC_PREREQ(2.59)
AC_INIT(melange, 0.1, melange-devel@recoil.org)
AC_CONFIG_SRCDIR([OCamlMakefile])
AC_CONFIG_HEADER([config.h])

AC_CONFIG_FILES([config.mk])

# Checks for programs.
AC_PROG_CC
AC_PROG_INSTALL

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([arpa/inet.h fcntl.h netdb.h netinet/in.h paths.h stdlib.h string.h sys/ioctl.h sys/socket.h termios.h unistd.h util.h pty.h])

AC_SEARCH_LIBS(openpty, util, [], [], [])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST

# Checks for library functions.
AC_PROG_GCC_TRADITIONAL
AC_CHECK_FUNCS([inet_ntoa memset strerror])

AC_ARG_ENABLE(debug, [  --enable-debug          output verbose debug messages], [DEBUG_MODE="DEBUG"], [DEBUG_MODE="NORMAL"])

AC_SUBST(DEBUG_MODE)
AC_OUTPUT
