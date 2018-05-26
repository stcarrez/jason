dnl Autoconf configure script for jason
dnl Process this file with autoconf to produce a configure script.
AC_INIT(src/jason.ads)
AC_CANONICAL_SYSTEM
dnl AM_MAINTAINER_MODE
# Current release settings
JASON_MAJOR_VERSION=0
JASON_MINOR_VERSION=2
JASON_MICRO_VERSION=0
JASON_VERSION=$JASON_MAJOR_VERSION.$JASON_MINOR_VERSION.$JASON_MICRO_VERSION

# checking for local tools
AC_PROG_CC
AC_PROG_MAKE_SET
AC_PROG_INSTALL
AC_PROG_LN_S
AC_CHECK_PROGS(GNATMAKE, gnatmake, "")

# Set the version number of GtkAda
AC_SUBST(JASON_VERSION)
AC_SUBST(JASON_MAJOR_VERSION)
AC_SUBST(JASON_MINOR_VERSION)
AC_SUBST(JASON_MICRO_VERSION)
EXEC_PREFIX="$prefix"
AC_SUBST(EXEC_PREFIX)
JASON_LIBDIR="lib"
AC_SUBST(JASON_LIBDIR)

AC_MSG_CHECKING([number of processors])
NR_CPUS=`getconf _NPROCESSORS_CONF 2>/dev/null || getconf NPROCESSORS_CONF 2>/dev/null || echo 1`
AC_MSG_RESULT($NR_CPUS)
AC_SUBST(NR_CPUS)

##########################################
# Ada Util library
##########################################
AM_GNAT_FIND_PROJECT([ada-util],[Ada Utility Library],[util],
  [git@github.com:stcarrez/ada-util.git],
  [Building jason requires the Ada Utility Library.],
  [
    UTIL_DIR=
  ])
AC_SUBST(UTIL_DIR)

##########################################
# Ada Expression Language library
##########################################
AM_GNAT_FIND_PROJECT([ada-el],[Ada Expression Language Library],[el],
  [git@github.com:stcarrez/ada-el.git],
  [Building jason requires the Ada EL Library.],
  [
    EL_DIR=
  ])
AC_SUBST(EL_DIR)

##########################################
# Ada Server Faces library
##########################################
AM_GNAT_FIND_PROJECT([ada-asf],[Ada Server Faces],[asf],
  [git@github.com:stcarrez/ada-asf.git],
  [Building jason requires the Ada Server Faces Library.],
  [
    ASF_DIR=
  ])
AC_SUBST(ASF_DIR)

##########################################
# Ada Security library
##########################################
AM_GNAT_FIND_PROJECT([ada-security],[Ada Security Library],[security],
  [git@github.com:stcarrez/ada-security.git],
  [Building jason requires the Ada Security Library.],
  [
    SECURITY_DIR=
  ])
AC_SUBST(SECURITY_DIR)

##########################################
# Ada Database Objects library
##########################################
AM_GNAT_FIND_PROJECT([ada-ado],[Ada Database Objects],[ado],
  [git@github.com:stcarrez/ada-ado.git],
  [Building jason requires the Ada Database Objects Library.],
  [
    ADO_DIR=
  ])
AC_SUBST(ADO_DIR)

##########################################
# Ada Web Application library
##########################################
AM_GNAT_FIND_PROJECT([ada-awa],[Ada Web Application],[awa],
  [git@github.com:stcarrez/ada-awa.git],
  [Building jason requires the Ada Web Application Library.],
  [
    AWA_DIR=
  ])
AC_SUBST(AWA_DIR)

AM_UTIL_CHECK_INSTALL

AC_OUTPUT(
Makefile jason.gpr jason_server.gpr jason_tests.gpr rules.mk
)