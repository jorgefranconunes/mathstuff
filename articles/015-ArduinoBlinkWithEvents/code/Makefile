###########################################################################
#
# Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
#
###########################################################################

B_PROJ_VERSION = 0.0.2

MODULES = \
	ardev-core \
	ardev-test \
	ardev-atm328p \
	demos/Blink \
	demos/HelloWorld



.PHONY : default all check clean

default : all

all :
	@for module in $(MODULES) ; do make -C $$module $@ || exit 1; done

check :
	@for module in $(MODULES) ; do make -C $$module $@ || exit 1; done

clean :
	@for module in $(MODULES) ; do make -C $$module $@ || exit 1; done





-include Makefile.conf

Makefile.conf : 
	@echo "***"
	@echo "*** File \"Makefile.conf\" is missing."
	@echo "*** See 00README-developers.txt for aditional details."
	@echo "***"
	@exit 1

