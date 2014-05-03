###########################################################################
#
# Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
#
###########################################################################

B_PROJ = dummy

B_PROJ_VERSION = 0.0.1





#
#
#
B_TARGET_DIR = target

B_SRC_MAIN_C   = src/main/c
B_SRC_MAIN_CXX = src/main/cpp

B_SRC_TEST_C   = src/test/c
B_SRC_TEST_CXX = src/test/cpp

B_CC = gcc
B_CC_MAIN = $(B_CC)
B_CC_TEST = $(B_CC)

B_CC_FLAGS =
B_CC_FLAGS_MAIN = $(B_CC_FLAGS)
B_CC_FLAGS_TEST = $(B_CC_FLAGS)

B_CXX = g++
B_CXX_MAIN = $(B_CXX)
B_CXX_TEST = $(B_CXX)

B_CXX_FLAGS =
B_CXX_FLAGS_MAIN = $(B_CXX_FLAGS)
B_CXX_FLAGS_TEST = $(B_CXX_FLAGS)

B_LD = g++
B_LD_MAIN = $(B_LD)
B_LD_TEST = $(B_LD)





#
# Tools used during the build.
#
B_MKDIR = mkdir -p
B_DELETE = rm -rf
B_AR = ar cru
B_RANLIB = ranlib





###########################################################################
#
# 
#
###########################################################################

