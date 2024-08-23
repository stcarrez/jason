NAME=jason
VERSION=1.0.0

DIST_DIR=jason-$(VERSION)
DIST_FILE=jason-$(VERSION).tar.gz

MAKE_ARGS += -XJASON_BUILD=$(BUILD)

-include Makefile.conf

STATIC_MAKE_ARGS = $(MAKE_ARGS) -XJASON_LIBRARY_TYPE=static
SHARED_MAKE_ARGS = $(MAKE_ARGS) -XJASON_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XUTILADA_BASE_BUILD=relocatable -XUTIL_LIBRARY_TYPE=relocatable
SHARED_MAKE_ARGS += -XXMLADA_BUILD=relocatable
SHARED_MAKE_ARGS += -XLIBRARY_TYPE=relocatable

include Makefile.defaults

# Build executables for all mains defined by the project.
build-test::
	cd regtests && $(BUILD_COMMAND) $(GPRFLAGS) $(MAKE_ARGS)

# Build and run the unit tests
test:	build
	bin/jason_harness -xml jason-aunit.xml

ROOTDIR=.

jason_dir=$(ROOTDIR)
jason_dynamo=--package Jason.Projects.Models \
  --package Jason.Tickets.Models \
  db uml/jason.zargo
jason_install_dirs=config db bundles web

$(eval $(call awa_plugin,jason))

