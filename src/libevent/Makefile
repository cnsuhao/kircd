CC=gcc
CFLAGS=-g -Wall
INCLUDE=-I./

AR=ar rcs
RM=rm -rf

COMMON_LIB=lib/common.a
COMMON_SRC_DIR=common
COMMON_OBJ_DIR=obj
COMMON_OBJS=$(patsubst $(COMMON_SRC_DIR)/%.c,$(COMMON_OBJ_DIR)/%.o,$(wildcard $(COMMON_SRC_DIR)/*.c))

CGREEN_PATH=3rd/cgreen
CGREEN_LIB=$(CGREEN_PATH)/cgreen.a

TEST_BIN=unittest
TEST_SRC_DIR=test
TEST_OBJ_DIR=testobj
TEST_OBJS=$(patsubst $(TEST_SRC_DIR)/%.c,$(TEST_OBJ_DIR)/%.o,$(wildcard $(TEST_SRC_DIR)/*.c))
TEST_INCLUDE=$(INCLUDE) -I$(CGREEN_PATH)


#--------------------------------------------------------------
# all

all: $(COMMON_LIB)
	gcc -g -Wall main.c common/kstring.c common/kutil.c

clean: clean_common clean_unittest
	$(RM) a.out


#--------------------------------------------------------------
# common

$(COMMON_OBJ_DIR)/%.o: $(COMMON_SRC_DIR)/%.c
	$(CC) $< -o $@ -c $(CFLAGS) $(INCLUDE)

$(COMMON_LIB): $(COMMON_OBJS)
	$(AR) $(COMMON_LIB) $(COMMON_OBJS)

clean_common:
	$(RM) $(COMMON_LIB) $(COMMON_OBJS)


#--------------------------------------------------------------
# unit test

$(TEST_OBJ_DIR)/%.o: $(TEST_SRC_DIR)/%.c
	$(CC) $< -o $@ -c $(CFLAGS) $(TEST_INCLUDE)

unittest: $(TEST_OBJS) $(CGREEN_LIB) $(COMMON_LIB)
	$(CC) $(TEST_OBJS) $(CGREEN_LIB) $(COMMON_LIB) -o $(TEST_BIN) $(CFLAGS)
	
clean_unittest:
	$(RM) $(TEST_BIN) $(TEST_OBJS)

run_unittest: test
	time $(TEST_BIN)


#--------------------------------------------------------------
# 3rd party

$(CGREEN_LIB):
	test -f $(CGREEN_LIB) || \
	(cd $(CGREEN_PATH); gmake)

