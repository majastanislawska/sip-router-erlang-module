# $Id$
#
# WARNING: do not run this directly, it should be run by the master Makefile

include ../../Makefile.defs
auto_gen=
NAME=db_erlang.so

DEFS +=-DSER_MOD_INTERFACE -D_REENTRANT
LIBS =-lerl_interface -lei -lpthread

SERLIBPATH=../../lib
SER_LIBS=$(SERLIBPATH)/srdb2/srdb2 $(SERLIBPATH)/srdb1/srdb1

ifneq (,$(filter sip-router ser, $(INSTALL_FLAVOUR)))
#MOD_INSTALL_SHARE= 
else ifeq ($(INSTALL_FLAVOUR),kamailio)
#TODO
endif

include ../../Makefile.modules

ifneq (,$(filter sip-router ser, $(INSTALL_FLAVOUR)))
#extra install for ser
else ifeq ($(INSTALL_FLAVOUR),kamailio)
# extra install for kamailio
endif # INSTALL_FLAVOUR
