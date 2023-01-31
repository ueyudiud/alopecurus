##############################################################
# Makefile for building Alopecurus
##############################################################

MAJVER=0
MINVER=1

##############################################################
# Build configuration.
##############################################################
LEVEL=release
TARGET=mixed
OUT=.

##############################################################
# Build tools.
##############################################################
CC= gcc -std=gnu11
AR= ar rcus
RM= rm -f
STRIP= strip

##############################################################
# Host OS detection.
##############################################################
# Supported OS list:
# Windows, Linux, Darwin
##############################################################

ifeq ($(OS),Windows_NT)
	HOST_OS= Windows
	RM= del /f
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

ifeq (Windows, $(TARGET_OS))
	ALO_T= alo.exe
	ifeq (cygwin,$(HOST_MSYS))
		ALO_SO= cyg$(TARGET_DLLNAME)
	else
		ALO_SO= $(TARGET_DLLNAME)
	endif
endif

##############################################################
# Build mode controlling.
##############################################################

TARGET_O= $(ALO_A)
TARGET_T= $(ALO_T) $(ALO_SO)

ifeq (static,$(TARGET))
	TARGET_DYCC= @:
	TARGET_T= $(ALO_T)
else
ifeq (dynamic,$(TARGET))
	ifneq (Windows,$(TARGET_OS))
		TARGET_CC= $(DYNAMIC_CC)
	endif
	TARGET_DYCC= @:
	TARGET_O= $(ALO_SO)
endif
endif

ifneq (debug,$(LEVEL))
  TARGET_STRIP= @:
endif

ACTX_C= actx/x64.w64.win.c

KER_O= aaux.o aapi.o adump.o acode.o aparse.o alex.o adbg.o \
aerr.o aload.o asave.o avm.o afmt.o astrx.o aop.o agc.o amem.o \
aenv.o afun.o atable.o alist.o atuple.o astr.o abuf.o aio.o \
actx.o
LIB_O= abaselib.o
BASE_O= $(KER_O) $(LIB_O)

ALO_O= alo.o

ALL_O= $(BASE_O) $(ALO_O)
ALL_T= $(TARGET_T)
ALL_RM= $(ALL_O) $(ALL_O:.o=_dyn.o) $(ALO_T) $(ALO_A) $(ALO_SO)

Q=@echo
E=@

##############################################################
# Make targets.
##############################################################

default all: $(ALL_T)
	$(Q) "Build success."

clean:
	$(Q) "RM      $(ALL_RM)"
	$(E) cd $(OUT) && $(HOST_RM) $(ALL_RM)

echo:
	$(Q) "   LEVEL=$(LEVEL)"
	$(Q) "      CC=$(CC)"
	$(Q) "  CFLAGS=$(BASE_CCFLAGS)"

depend:
	$(E) $(HOST_CC) $(HOST_CCFLAGS) -MM *.c > makefile.dep

.PHONY: default all clean depend

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
	$(E) $(TARGET_DYCC) $(TARGET_CCFLAGS) -o "$(OUT)/actx_dyn.o" -c $<
	$(E) $(TARGET_CC) $(TARGET_CCFLAGS) -o "$(OUT)/actx.o" -c $<

actx.h: actx.o

include makefile.dep

##############################################################
# Target file rules.
##############################################################

$(ALO_A): $(BASE_O)
	$(Q) "AR      $@"
	$(E) $(TARGET_AR) "$(OUT)/$@" $(addprefix $(OUT)/, $(BASE_O))

$(ALO_SO): $(BASE_O)
	$(Q) "DYLINK  $@"
	$(E) cd $(OUT) && $(TARGET_LD) $(TARGET_SHLDFLAGS) -o $@ $(BASE_O) $(TARGET_LIBS)
	$(E) cd $(OUT) && $(TARGET_STRIP) $@

$(ALO_T): $(TARGET_O) $(ALO_O)
	$(Q) "LINK    $@"
	$(E) cd $(OUT) && $(TARGET_LD) $(TARGET_LDFLAGS) -o $@ $(ALO_O) $(TARGET_O) $(TARGET_LIBS)

##############################################################
