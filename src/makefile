##############################################################
# Makefile for building Alopecurus
##############################################################
MAJVER=0
MINVER=1
PATVAR=0

##############################################################
# Build configuration.
##############################################################
LEVEL=release
BUILDMODE=mixed
DEBUGMODE=gdb
OUT=.

##############################################################
# Build tools.
##############################################################
CC= gcc -std=gnu11
AR= ar rcus
STRIP= strip

RM= rm -f $(1)
MD= mkdir -p $(1)
CP= cp -f $(1) $(2)

##############################################################
# Host OS detection.
##############################################################
# Supported OS list:
# Windows
##############################################################
# TODO OS list:
# Linux, Darwin
##############################################################

ifeq ($(OS),Windows_NT)
	HOST_OS= Windows
	RM= del /f $(1)
	MD= if not exist $(1) mkdir $(1)
	CP= copy /Y $(1) $(2)
else
	HOST_OS:= $(shell uname -s)
	ifneq (,$(findstring MINGW,$(OS)))
		HOST_OS= Windows
		HOST_MSYS= mingw
	endif
	ifneq (,$(findstring CYGWIN,$(OS)))
		HOST_OS= Windows
		HOST_MSYS= cygwin
	endif
endif

##############################################################
# Flags and options.
##############################################################

CCOPT=
CCDEBUG=
CCWARN= -Wall -Wextra
CCDEF=
CCFLAGS=
LIBS= -lm

ifeq (release,$(LEVEL))
	CCOPT= -O2 -fomit-frame-pointer
	CCDEBUG= -g0
else
	ifeq (debug,$(LEVEL))
		CCOPT= -O0
		CCDEBUG= -g3
  		CCDEF+= -D ALOI_DEBUG
	endif
endif

BASE_CC= $(CC)
BASE_CCFLAGS= $(CCDEBUG) $(CCOPT) $(CCWARN) $(CCDEF) $(CCFLAGS)
BASE_LDFLAGS= $(CCDEBUG) $(LDFLAGS)
BASE_XLIBS= $(LIBS)

HOST_CCLIB=
HOST_CCSYS=
HOST_CCDEF=
HOST_XLIBS=

HOST_CC?= $(BASE_CC)
HOST_RM= $(RM)
HOST_MD= $(MD)
HOST_CP= $(CP)
HOST_AR= $(AR)

HOST_XCCFLAGS= $(HOST_CCSYS) $(HOST_CCDEF)

HOST_CCFLAGS= $(BASE_CCFLAGS) $(HOST_XCCFLAGS)
HOST_LIBS= $(BASE_XLIBS) $(HOST_XLIBS)

##############################################################
# Target system detection.
##############################################################

CROSS=
TARGET_OS?= $(HOST_OS)

STATIC_CC= $(CROSS)$(CC)
DYNAMIC_CC= $(STATIC_CC) -fPIC

TARGET_CCSYS=
TARGET_CCDEF=
TARGET_XLIBS=

TARGET_STCC= $(STATIC_CC)
TARGET_DYCC= $(DYNAMIC_CC)
TARGET_CC= $(TARGET_STCC)
TARGET_LD= $(CROSS)$(CC)
TARGET_AR= $(CROSS)$(AR)
TARGET_STRIP= $(CROSS)$(STRIP)

TARGET_SONAME= libalo.so.$(MAJVER).$(MINVER)
TARGET_DYLIBNAME= libalo.$(MAJVER).$(MINVER).dylib
TARGET_DLLNAME= alo$(MAJVER)$(MINVER).dll

TARGET_FLAGS=
TARGET_XCCFLAGS= $(TARGET_CCSYS) $(TARGET_CCDEF)
TARGET_XLDFLAGS=
TARGET_XDYLDFLAGS= -shared -fPIC -Wl,-soname,$(TARGET_SONAME)

TARGET_CCFLAGS= $(BASE_CCFLAGS) $(TARGET_FLAGS) $(TARGET_XCCFLAGS)
TARGET_LDFLAGS= $(BASE_LDFLAGS) $(TARGET_FLAGS) $(TARGET_XLDFLAGS)
TARGET_SHLDFLAGS= $(BASE_LDFLAGS) $(TARGET_FLAGS) $(TARGET_XDYLDFLAGS)
TARGET_LIBS= $(BASE_XLIBS) $(TARGET_XLIBS)

ifeq (Windows,$(TARGET_OS))
  	TARGET_STRIP+= --strip-unneeded
  	TARGET_XDYLDFLAGS= -shared
	TARGET_XLIBS+= -lntdll
else 
ifeq (Darwin,$(TARGET_OS))
	TARGET_STRIP+= -x
endif
endif

##############################################################
# File and path.
##############################################################

ALO_O= alo.o
ALO_A= alo.a
ALO_SO= alo.so
ALO_T= alo
ALO_DB= $(TARGET_SONAME)-$(DEBUGMODE).py
ALO_DB_SRC=alo-$(DEBUGMODE).py

ifeq (Windows,$(TARGET_OS))
	ALO_T= alo.exe
	ifeq (cygwin,$(HOST_MSYS))
		ALO_SO= cyg$(TARGET_DLLNAME)
	else
		ALO_SO= $(TARGET_DLLNAME)
	endif
	ALO_DB= $(ALO_SO)-$(DEBUGMODE).py
endif

##############################################################
# Build mode controlling.
##############################################################
TARGET_O= $(ALO_A)
TARGET_T= $(ALO_T) $(ALO_SO)

ifeq (Windows,$(TARGET_OS))
	TARGET_DYCC= $(STATIC_CC)
	ifneq (static,$(BUILDMODE))
		BUILDMODE= dynamic
		TARGET_XCCFLAGS+= -D ALO_BUILD_AS_DLL
	endif
endif

ifeq (static,$(BUILDMODE))
	TARGET_DYCC= @:
	TARGET_T= $(ALO_T)
else
ifeq (dynamic,$(BUILDMODE))
	ifneq (Windows,$(TARGET_OS))
		TARGET_CC= $(DYNAMIC_CC)
	endif
	TARGET_DYCC= @:
	TARGET_O= $(ALO_SO)
endif
endif

ifeq (debug,$(LEVEL))
  TARGET_STRIP= @:
  ifneq (static,$(BUILDMODE))
  	TARGET_T+= $(ALO_DB)
  endif
endif

ACTX_C= actx/x64.w64.win.c

KER_O= aapi.o adump.o acode.o aparse.o alex.o adbg.o aerr.o aload.o \
asave.o avm.o afmt.o astrx.o aop.o agc.o amem.o aenv.o amod.o ameta.o \
afun.o atable.o alist.o atuple.o astr.o adict.o abuf.o aio.o actx.o
LIB_O= asyslib.o adbglib.o abaselib.o aauxlib.o
BASE_O= $(KER_O) $(LIB_O)

ALO_O= alo.o

ALL_O= $(BASE_O) $(ALO_O)
ALL_T= $(TARGET_T)
ALL_RM= $(ALL_O) $(ALL_O:.o=_dyn.o) $(ALO_T) $(ALO_A) $(ALO_SO) $(ALO_DB)

E=@
Q=@echo
#E=
#Q=@:

##############################################################
# Make targets.
##############################################################

default all: $(ALL_T)
	$(Q) "Build success."

clean:
	$(Q) "RM      $(ALL_RM)"
	$(E) cd $(OUT) && $(call HOST_RM, $(ALL_RM))

echo:
	$(Q) "    LEVEL=$(LEVEL)"
	$(Q) "BUILDMODE=$(BUILDMODE)"
	$(Q) "DEBUGMODE=$(DEBUGMODE)"

depend:
	$(E) $(HOST_CC) $(HOST_CCFLAGS) -MM *.c > makefile.dep

check:
	$(Q) "MKDIR   $(OUT)"
	$(E) $(call MD, $(OUT))

.PHONY: default all clean depend check

##############################################################
# Object file rules.
##############################################################

%.o: %.c
	$(Q) "CC      $@"
	$(E) $(TARGET_DYCC) $(TARGET_CCFLAGS) -o "$(OUT)/$(@:.o=_dyn.o)" -c $<
	$(E) $(TARGET_CC) $(TARGET_CCFLAGS) -o "$(OUT)/$@" -c $<

$(ALO_O): alo.c
	$(Q) "CC      $@"
	$(E) $(TARGET_CC) $(TARGET_CCFLAGS) -o "$(OUT)/$@" -c $<

actx.o: $(ACTX_C)
	$(Q) "CC      $@"
	$(E) $(TARGET_DYCC) $(TARGET_CCFLAGS) -o "$(OUT)/$(@:.o=_dyn.o)" -c $<
	$(E) $(TARGET_CC) $(TARGET_CCFLAGS) -o "$(OUT)/$@" -c $<

actx.h: actx.o

include makefile.dep

##############################################################
# Target file rules.
##############################################################

$(ALO_A): $(BASE_O)
	$(Q) "AR      $@"
	$(E) cd $(OUT) && $(TARGET_AR) $@ $(BASE_O)

ifneq (debug,$(BUILDMODE))
$(ALO_DB): $(ALO_DB_SRC) check
	$(Q) "CP      $(ALO_DB_SRC) $@"
	$(E) $(call CP, $(ALO_DB_SRC), "$(OUT)/$@")
endif

$(ALO_SO): $(BASE_O)
	$(Q) "DYLINK  $@"
	$(E) cd $(OUT) && $(TARGET_LD) $(TARGET_SHLDFLAGS) -o $@ $(BASE_O) $(TARGET_LIBS)
	$(E) cd $(OUT) && $(TARGET_STRIP) $@

$(ALO_T): $(TARGET_O) $(ALO_O)
	$(Q) "LINK    $@"
	$(E) cd $(OUT) && $(TARGET_LD) $(TARGET_LDFLAGS) -o $@ $(ALO_O) $(TARGET_O) $(TARGET_LIBS)

##############################################################