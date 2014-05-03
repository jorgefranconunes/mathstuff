###########################################################################
#
# Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
#
#
# The following variables must be defined:
#
# B_PROJ
#
# B_PROJ_VERSION
#
# B_TARGET_DIR
#
# B_SRC_MAIN_C
#
# B_SRC_MAIN_CXX
#
# B_SRC_TEST_C
#
# B_SRC_TEST_CXX
#
#
###########################################################################





B_TARGET_MAIN_BASENAME = lib$(B_PROJ)-$(B_PROJ_VERSION).a
B_TARGET_MAIN          = $(B_TARGET_DIR)/$(B_TARGET_MAIN_BASENAME)

B_TARGET_TEST = $(B_TARGET_DIR)/AllTests





.PHONY : default all check clean

default : all

all : $(B_TARGET_MAIN)

check : $(B_TARGET_TEST)
	$(B_TARGET_TEST)

clean :
	$(B_DELETE) $(B_TARGET_DIR)





###########################################################################
#
# Rules for compiling the main source code and creating the library
# archive.
#
###########################################################################

B_OBJ_MAIN = $(B_TARGET_DIR)/main_o

B_OBJ_MAIN_C   = $(B_OBJ_MAIN)
B_OBJ_MAIN_CXX = $(B_OBJ_MAIN)

B_OBJ_MAIN_C_FILES   = $(B_MAIN_C_SRC:$(B_SRC_MAIN_C)/%.c=$(B_OBJ_MAIN_C)/%.o)
B_OBJ_MAIN_CXX_FILES = $(B_MAIN_CXX_SRC:$(B_SRC_MAIN_CXX)/%.cpp=$(B_OBJ_MAIN_CXX)/%.o)

B_OBJ_MAIN_ALL_FILES = $(B_OBJ_MAIN_C_FILES) $(B_OBJ_MAIN_CXX_FILES)
B_DEPS_MAIN_ALL_FILES = $(B_OBJ_MAIN_ALL_FILES:%.o=%.o.d)


B_MY_CC_FLAGS_MAIN = \
	-I $(B_SRC_MAIN_C) \
	 $(B_CC_FLAGS_MAIN)

B_MY_CXX_FLAGS_MAIN = \
	-I $(B_SRC_MAIN_CXX) \
	-I $(B_SRC_MAIN_C) \
	$(B_CXX_FLAGS_MAIN) 


$(B_TARGET_MAIN) : $(B_OBJ_MAIN_ALL_FILES)
	$(B_AR) $(B_TARGET_MAIN) $(B_OBJ_MAIN_ALL_FILES)
	$(B_RANLIB) $(B_TARGET_MAIN)

$(B_OBJ_MAIN_C)/%.o : $(B_SRC_MAIN_C)/%.c
	$(B_MKDIR) $(@D)
	$(B_CC_MAIN) -c $(B_MY_CC_FLAGS_MAIN) -MT $@ -MD -MP -MF $@.d -o $@ $<

$(B_OBJ_MAIN_CXX)/%.o : $(B_SRC_MAIN_CXX)/%.cpp
	$(B_MKDIR) $(@D)
	$(B_CXX_MAIN) -c $(B_MY_CXX_FLAGS_MAIN) -MT $@ -MD -MP -MF $@.d -o $@ $<

# Include the generated dependencies. But we do not worry if they do
# not exist yet.
-include $(B_DEPS_MAIN_ALL_FILES)





###########################################################################
#
# Rules for compiling the test source code and creating the unit tests
# runner application.
#
###########################################################################

B_OBJ_TEST = $(B_TARGET_DIR)/test_o

B_OBJ_TEST_C   = $(B_OBJ_TEST)
B_OBJ_TEST_CXX = $(B_OBJ_TEST)

B_OBJ_TEST_C_FILES   = $(B_TEST_C_SRC:$(B_SRC_TEST_C)/%.c=$(B_OBJ_TEST_C)/%.o)
B_OBJ_TEST_CXX_FILES = $(B_TEST_CXX_SRC:$(B_SRC_TEST_CXX)/%.cpp=$(B_OBJ_TEST_CXX)/%.o)

B_OBJ_TEST_ALL_FILES = $(B_OBJ_TEST_C_FILES) $(B_OBJ_TEST_CXX_FILES)
B_DEPS_TEST_ALL_FILES = $(B_OBJ_TEST_ALL_FILES:%.o=%.o.d)


B_MY_CC_FLAGS_TEST = \
	-I $(B_SRC_TEST_C) \
	-I $(B_SRC_MAIN_C) \
	 $(B_CC_FLAGS_TEST)

B_MY_CXX_FLAGS_TEST = \
	-I $(B_SRC_TEST_CXX) \
	-I $(B_SRC_TEST_CXX) \
	-I $(B_SRC_TEST_C) \
	-I $(B_SRC_MAIN_C) \
	 $(B_CXX_FLAGS_TEST)

B_MY_LD_TEST_FLAGS = \
	-L$(B_TARGET_DIR) \
	-l $(B_PROJ)-$(B_PROJ_VERSION) \
	$(B_LD_FLAGS_TEST)


$(B_TARGET_TEST) : $(B_TARGET_MAIN) $(B_OBJ_TEST_ALL_FILES)
	$(B_LD_TEST) -o $@ $(B_OBJ_TEST_ALL_FILES) $(B_MY_LD_TEST_FLAGS)

$(B_OBJ_TEST_C)/%.o : $(B_SRC_TEST_C)/%.c
	$(B_MKDIR) $(@D)
	$(B_CC_TEST) -c $(B_MY_CC_FLAGS_TEST) -MT $@ -MD -MP -MF $@.d -o $@ $<

$(B_OBJ_TEST_CXX)/%.o : $(B_SRC_TEST_CXX)/%.cpp
	$(B_MKDIR) $(@D)
	$(B_CXX_TEST) -c $(B_MY_CXX_FLAGS_TEST) -MT $@ -MD -MP -MF $@.d -o $@ $<

# Include the generated dependencies. But we do not worry if they do
# not exist yet.
-include $(B_DEPS_TEST_ALL_FILES)





###########################################################################
#
# 
#
###########################################################################

