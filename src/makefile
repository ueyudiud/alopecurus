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

ifeq (Windows,$(findstring Windows,$(OS)))
	HOST_OS= Windows
	SHELL= cmd
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
	TARGET_CCDEF+= -DALOI_USE_READLINE
	TARGET_LIBS+= -lreadline
endif

##############################################################
# File and path.
##############################################################

ALO_O= alo.o
ALO_A= alo.a
ALO_SO= alo.so
ALO_T= alo

ifeq (Windows,$(TARGET_OS))
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

ifeq (Windows,$(TARGET_OS))
	TARGET_DYCC= $(STATIC_CC)
	ifneq (static,$(BUILDMODE))
		BUILDMODE= dynamic
		TARGET_XCCFLAGS+= -DALO_BUILD_AS_DLL
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
endif

KER_O= aapi.o aparse.o alex.o adbg.o aerr.o aload.o asave.o avm.o \
atm.o afmt.o agc.o amem.o aenv.o astk.o auser.o atype.o afun.o \
atable.o alist.o atuple.o astr.o abuf.o aio.o aobj.o abc.o actx.o \
ajmp.o
LIB_O= asyslib.o adbglib.o atypelib.o abaselib.o aauxlib.o
BASE_O= $(KER_O) $(LIB_O)

ifeq (Windows,$(TARGET_OS))
	ACTX_C= actx/x64.w64.win.c
	ACTX_S= actx/x64.w64.win.s
	BASE_DYNO= $(BASE_O)
else
	ACTX_C= actx/x64.sysv.posix.c
	ACTX_S= actx/x64.sysv.posix.s
	BASE_DYNO= $(BASE_O:.o=_dyn.o)
endif

ALO_O= alo.o

ALL_O= $(BASE_O) $(ALO_O)
ALL_T= $(TARGET_T)
ALL_RM= $(ALL_O) $(BASE_DYNO) $(ALO_T) $(ALO_A) $(ALO_SO)

#E=@
#Q=@echo
E=
Q=@:

##############################################################
# Make targets.
##############################################################

default all: $(ALL_T)
	$(Q) "Build success."

clean:
	$(Q) "RM      $(ALL_RM)"
	$(E) $(call HOST_RM, $(ALL_RM))

echo:
	$(Q) "    LEVEL=$(LEVEL)"
	$(Q) "BUILDMODE=$(BUILDMODE)"
	$(Q) "DEBUGMODE=$(DEBUGMODE)"
	$(Q) "       OS=$(TARGET_OS)"

depend:
	$(E) $(HOST_CC) $(HOST_CCFLAGS) -MM *.c > makefile.dep

.PHONY: default all clean depend check

##############################################################
# Object file rules.
##############################################################

%.o: %.c
	$(Q) "CC      $@"
	$(E) $(TARGET_DYCC) $(TARGET_CCFLAGS) -o $(@:.o=_dyn.o) -c $<
	$(E) $(TARGET_CC) $(TARGET_CCFLAGS) -o $@ -c $<

$(ALO_O): alo.c
	$(Q) "CC      $@"
	$(E) $(TARGET_CC) $(TARGET_CCFLAGS) -o $@ -c $<

actx.o: $(ACTX_C)
	$(Q) "CC      $@"
	$(E) $(TARGET_DYCC) $(TARGET_CCFLAGS) -o $(@:.o=_dyn.o) -c $<
	$(E) $(TARGET_CC) $(TARGET_CCFLAGS) -o $@ -c $<

ajmp.o: $(ACTX_S)
	$(Q) "CC      $@"
	$(E) $(TARGET_DYCC) $(TARGET_CCFLAGS) -o $(@:.o=_dyn.o) -c $<
	$(E) $(TARGET_CC) $(TARGET_CCFLAGS) -o $@ -c $<

actx.h: actx.o ajmp.o

include makefile.dep

##############################################################
# Target file rules.
##############################################################

$(ALO_A): $(BASE_O)
	$(Q) "AR      $@"
	$(E) $(TARGET_AR) $@ $(BASE_O)

$(ALO_SO): $(BASE_O)
	$(Q) "DYLINK  $@"
	$(E) $(TARGET_LD) $(TARGET_SHLDFLAGS) -o $@ $(BASE_DYNO) $(TARGET_LIBS)
	$(E) $(TARGET_STRIP) $@

$(ALO_T): $(TARGET_O) $(ALO_O)
	$(Q) "LINK    $@"
	$(E) $(TARGET_LD) $(TARGET_LDFLAGS) -o $@ $(ALO_O) $(TARGET_O) $(TARGET_LIBS)

##############################################################
