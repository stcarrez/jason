dnl Autoconf configure script for jason
dnl Process this file with autoconf to produce a configure script.
AC_INIT(src/jason.ads)

# Current release settings
JASON_MAJOR_VERSION=0
JASON_MINOR_VERSION=2
JASON_MICRO_VERSION=0
JASON_VERSION=$JASON_MAJOR_VERSION.$JASON_MINOR_VERSION.$JASON_MICRO_VERSION

# Set the version number of GtkAda
AC_SUBST(JASON_VERSION)
AC_SUBST(JASON_MAJOR_VERSION)
AC_SUBST(JASON_MINOR_VERSION)
AC_SUBST(JASON_MICRO_VERSION)

EXEC_PREFIX="$prefix"
AC_SUBST(EXEC_PREFIX)

AM_GNAT_LIBRARY_PROJECT(jason)

##########################################
# Ada Util library
##########################################
AM_GNAT_FIND_PROJECT([ada-util],[Ada Utility Library],[utilada_sys],
  [git@github.com:stcarrez/ada-util.git],
  [Building jason requires the Ada Utility Library.],
  [
    UTIL_DIR=${gnat_project_dir_utilada_sys}
  ])
AC_SUBST(UTIL_DIR)

##########################################
# Ada Expression Language library
##########################################
AM_GNAT_FIND_PROJECT([ada-el],[Ada Expression Language Library],[elada],
  [git@github.com:stcarrez/ada-el.git],
  [Building jason requires the Ada EL Library.],
  [
    EL_DIR=${gnat_project_dir_elada}
  ])
AC_SUBST(EL_DIR)

##########################################
# Ada Servlet library
##########################################
AM_GNAT_FIND_PROJECT([ada-servlet],[Ada Servlet],[servletada],
  [git@github.com:stcarrez/ada-servlet.git],
  [Building jason requires the Ada Servlet Library.],
  [
    SERVLET_DIR=${gnat_project_dir_servletada}
  ])
AC_SUBST(SERVLET_DIR)

##########################################
# Ada Server Faces library
##########################################
AM_GNAT_FIND_PROJECT([ada-asf],[Ada Server Faces],[asf],
  [git@github.com:stcarrez/ada-asf.git],
  [Building jason requires the Ada Server Faces Library.],
  [
    ASF_DIR=${gnat_project_dir_asf}
  ])
AC_SUBST(ASF_DIR)

##########################################
# Ada Security library
##########################################
AM_GNAT_FIND_PROJECT([ada-security],[Ada Security Library],[security],
  [git@github.com:stcarrez/ada-security.git],
  [Building jason requires the Ada Security Library.],
  [
    SECURITY_DIR=${gnat_project_dir_security}
  ])
AC_SUBST(SECURITY_DIR)

##########################################
# Ada Database Objects library
##########################################
AM_GNAT_FIND_PROJECT([ada-ado],[Ada Database Objects],[ado],
  [git@github.com:stcarrez/ada-ado.git],
  [Building jason requires the Ada Database Objects Library.],
  [
    ADO_DIR=${gnat_project_dir_ado}
  ])
AC_SUBST(ADO_DIR)

##########################################
# Ada Wiki library
##########################################
AM_GNAT_FIND_PROJECT([ada-wiki],[Ada Wiki],[wikiada],
  [git@github.com:stcarrez/ada-wiki.git],
  [Building jason requires the Ada Wiki Library.],
  [
    WIKI_DIR=${gnat_project_dir_wikiada}
  ])
AC_SUBST(WIKI_DIR)

##########################################
# Ada Web Application library
##########################################
AM_GNAT_FIND_PROJECT([ada-awa],[Ada Web Application],[awa],
  [git@github.com:stcarrez/ada-awa.git],
  [Building jason requires the Ada Web Application Library.],
  [
    AWA_DIR=${gnat_project_dir_awa}
  ])
AC_SUBST(AWA_DIR)

AM_UTIL_CHECK_INSTALL

AC_OUTPUT(
Makefile.conf jason.gpr jason_server.gpr jason_tests.gpr rules.mk
)
