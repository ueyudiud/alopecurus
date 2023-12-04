/**
 *@file aloadlib.c
 */

#define aloadlib_c_
#define ALO_LIB

#include "aarch.h"

#if ALO_OS_WINDOWS
# include <winerror.h>
# include <windows.h>
#endif

#include "alist.h"
#include "atable.h"
#include "amem.h"
#include "agc.h"
#include "avm.h"
#include "aapi.h"
#include "astrlib.h"

#include "alo.h"
#include "aauxlib.h"
#include "alolib.h"

#if ALO_OS_WINDOWS

#define CLIB_PATH_DEFAULT "./?.dll"
#define ALIB_PATH_DEFAULT "./?.alo", "./?/mod.alo"

typedef struct GLib {
    GOBJ_STRUCT_HEADER;
    HMODULE _handle;
} GLib;

static VTable const lib_vtable;

static void lib_drop(Global* gbl, GLib* self) {
    FreeLibrary(self->_handle);
    ai_mem_dealloc(gbl, self, sizeof(GLib));
}

static void lib_mark(Global* gbl, GLib* self) {
    (void) self;
    ai_gc_trace_work(gbl, sizeof(GLib));
}

static a_msg lib_error(a_henv env) {
    DWORD error = GetLastError();
    if (error == ERROR_MOD_NOT_FOUND) return ALO_EEMPTY;

    CHAR buf[128];
    if (FormatMessageA(FORMAT_MESSAGE_IGNORE_INSERTS | FORMAT_MESSAGE_FROM_SYSTEM,
                       NULL, error, 0,
                       buf, sizeof(buf) / sizeof(char), NULL))
        alo_pushntstr(env, buf);
    else
        alo_pushfstr(env, "system error %d", error);
    return ALO_EINVAL;
}

static a_msg lib_open(a_henv env, char const* file, GLib** plib) {
    GLib* self = ai_mem_alloc(env, sizeof(GLib));

    HMODULE handle = LoadLibrary(file);
    if (unlikely(handle == NULL)) {
        ai_mem_dealloc(G(env), self, sizeof(GLib));
        return lib_error(env);
    }
    self->_handle = handle;

    ai_gc_register_object(env, self);
    v_set_obj(env, api_incr_stack(env), self);

    *plib = self;
    return ALO_SOK;
}

static a_msg lib_load(unused a_henv env, GLib* self, char const* func, a_cfun* pproc) {
    void* addr = GetProcAddress(self->_handle, func);
    if (addr == null) return lib_error(env);
    a_cfun proc = cast(a_cfun, addr);
    *pproc = proc;
    return ALO_SOK;
}

static GLib* lib_cast(Value v) {
    if (!v_is_obj(v))
        return null;
    GObj* obj = v_as_obj(v);
    if (obj->_vptr != &lib_vtable)
        return null;
    return g_cast(GLib, obj);
}

static VTable const lib_vtable = {
    ._stencil = V_STENCIL(T_USER),
    ._tag = ALO_TUSER,
    ._flags = VTABLE_FLAG_GREEDY_MARK,
    ._slots = {
        [vfp_slot(drop)] = lib_drop,
        [vfp_slot(mark)] = lib_mark
    }
};

#else

#define CLIB_PATH_DEFAULT
#define ALIB_PATH_DEFAULT "./?.alo", "./?/mod.alo"

typedef GObj GLib;

#define LIB_ERR "dynamic library not enabled"

#define lib_drop null

static GLib* lib_open(unused a_henv env, unused char const* file) {
    alo_pushstr(env, LIB_ERR);
    return null;
}

static a_cfun lib_load(unused a_henv env, unused GLib* self, unused char const* func_name) {
    alo_pushstr(env, LIB_ERR);
    return null;
}

#endif

#define LOADED_FIELD_NAME "loaded"
#define LOADER_FIEND_NAME "loaders"
#define CLIB_PATH_FIEND_NAME "cpath"
#define CLIB_CACHE_FIELD_NAME "clibs"
#define CLIB_OPEN_FUNC_PREFIX "aloopen_"
#define ALIB_PATH_FIELD_NAME "path"
#define PATH_PLACEHOLDER "?"

#define DIR_SEP '/'
#define VER_SEP '-'

#define CAPTURED_SELF_INDEX ALO_STACK_INDEX_CAPTURE(0)

static GMod* check_self(a_henv env, a_ilen id) {
    aloL_checktag(env, id, ALO_TMOD);
    return v_as_mod(api_elem(env, id));
}

/*=========================================================*
 * 'loadlib' function support
 *=========================================================*/

static a_noret bad_state(a_henv env) {
    aloL_raisef(env, "invalid module loader state.");
}

static GTable* check_ccache(a_henv env, GMod* self) {
    Value v;
    catch (ai_mod_getls(env, self, CLIB_CACHE_FIELD_NAME, strlen(CLIB_CACHE_FIELD_NAME), &v) || !v_as_table(v)) {
        bad_state(env);
    }
    return v_as_table(v);
}

static a_msg load_clib(a_henv env, GMod* self, char const* file, GLib** plib) {
    GLib* lib;
    GTable* cache = check_ccache(env, self);

    Value* pv = ai_table_refls(env, cache, file, strlen(file));
    if (v_is_nil(*pv)) {
        try (lib_open(env, file, &lib));

        v_set_obj(env, pv, lib);

        ai_gc_barrier_forward(env, cache, lib);
    }
    else {
        lib = lib_cast(*pv);
        if (lib == null) {
            alo_pushfstr(env, "library object expected for file '%s'", file);
            return ALO_EINVAL;
        }
    }

    *plib = lib;
    return ALO_SOK;
}

static a_msg load_cfunc(a_henv env, GMod* self, char const* file, char const* func) {
    GLib* lib;
    try (load_clib(env, self, file, &lib));

    a_cfun proc;
    try (lib_load(env, lib, func, &proc));

    alo_newcfun(env, proc, 0);
    return ALO_SOK;
}

/*=========================================================*
 * 'use' function support
 *=========================================================*/

static GList* check_list(a_henv env, GMod* self, char const* str) {
    Value v;
    catch (ai_mod_getls(env, self, str, strlen(str), &v) || !v_is_list(v)) {
        bad_state(env);
    }
    return v_as_list(v);
}

static char const* check_path(a_henv env, GList* paths, a_u32 i) {
    Value v = paths->_ptr[i];
    if (!v_is_str(v)) aloL_raisef(env, "library path must be string", v_nameof(env, v));
    return str2ntstr(v_as_str(v));
}

typedef struct {
    char const* _file;
    char const* _proc;
} CLibPath;

static void resolve_clib_path(a_henv env, char const* base, char const* file, CLibPath* out) {
    /* File format like: X/Y-Z */
    out->_file = aloS_replace(env, base, PATH_PLACEHOLDER, file);

    char const* p1 = strrchr(file, DIR_SEP) ?: file;
    char const* p2 = strchr(p1, VER_SEP);
    out->_proc = p2 != null ?
        alo_pushfstr(env, "%s%.*s", CLIB_OPEN_FUNC_PREFIX, p2 - p1, p1) :
        alo_pushfstr(env, "%s%s", CLIB_OPEN_FUNC_PREFIX, p1);
}

static a_msg check_stat(a_henv env, a_msg msg) {
    catch (msg) {
        aloL_pushfail(env);
        alo_push(env, -2);
        return 2;
    }
    return 1;
}

static a_msg loader_clib(a_henv env) {
    char const* file = aloL_checkstr(env, 0);
    alo_settop(env, 1);

    GMod* self = check_self(env, CAPTURED_SELF_INDEX);
    GList* paths = check_list(env, self, CLIB_PATH_FIEND_NAME);

    aloL_Buf* buf = aloL_newbuf(env);

    for (a_u32 i = 0; i < paths->_len; ++i) {
        char const* base = check_path(env, paths, i);
        CLibPath path;
        resolve_clib_path(env, base, file, &path);

        a_msg msg = load_cfunc(env, self, path._file, path._proc);
        if (msg != ALO_EEMPTY) return check_stat(env, msg);

        alo_pushfstr(env, "\n\tno file '%s'", path._file);
        aloL_bufpush(env, buf);

        alo_settop(env, 2);
    }

    aloL_bufstr(env, buf);
    return 1;
}

static a_msg loader_alib(a_henv env) {
    char const* file = aloL_checkstr(env, 0);
    alo_settop(env, 1);

    GMod* self = check_self(env, CAPTURED_SELF_INDEX);
    GList* paths = check_list(env, self, ALIB_PATH_FIELD_NAME);

    aloL_Buf* buf = aloL_newbuf(env);

    for (a_u32 i = 0; i < paths->_len; ++i) {
        char const* base = check_path(env, paths, i);
        char const* path = aloS_replace(env, base, PATH_PLACEHOLDER, file);

        a_msg msg = aloL_compilef(env, path, ALO_COMP_OPT_MODULE);
        if (msg != ALO_EEMPTY) return check_stat(env, msg);

        alo_pushfstr(env, "\n\tno file '%s'", alo_tostr(env, -1));
        aloL_bufpush(env, buf);

        alo_settop(env, 2);
    }

    aloL_bufstr(env, buf);
    return 1;
}

static GTable* check_cache(a_henv env, GMod* self) {
    Value* pv = ai_mod_refls(env, self, LOADED_FIELD_NAME, strlen(LOADED_FIELD_NAME));

    GTable* cache;
    if (v_is_nil(*pv)) {
        cache = ai_table_new(env);

        v_set_obj(env, pv, cache);
        ai_gc_barrier_forward(env, self, cache);
    }
    else if (!v_is_table(*pv)) {
        bad_state(env);
    }
    else {
        cache = v_as_table(*pv);
    }

    return cache;
}

static a_noret load_error(a_henv env, a_ilen id, char const* name) {
    char const* cause = alo_isstr(env, id) ? alo_tostr(env, id) : "unknown error";
    aloL_raisef(env, "error loading module '%s':\n\t%s", name, cause);
}

static void load_module(a_henv env, GMod* self, GStr* name) {
    GList* loaders = check_list(env, self, LOADER_FIEND_NAME);
    aloL_Buf* buf = aloL_newbuf(env);

    for (a_u32 i = 0; i < loaders->_len; ++i) {
        /* TODO Should we protect loader in this scope? */
        vm_push_args(env, loaders->_ptr[i], v_of_str(name));

        catch (alo_pcall(env, 1, 2, 1)) {
            load_error(env, 1, str2ntstr(name));
        }

        if (alo_isfunc(env, 2)) {
            return;
        }
        else if (alo_isnil(env, 2)) {
            load_error(env, 3, str2ntstr(name));
        }
        else if (alo_isstr(env, 2)) {
            alo_settop(env, 3);
            aloL_bufpush(env, buf);
        }
    }

    aloL_raisef(env, "module '%s' not found:%.*s", str2ntstr(name), buf->len, buf->ptr);
}

static a_msg load_load(a_henv env) {
    GMod* self = check_self(env, CAPTURED_SELF_INDEX);
    GTable* cache = check_cache(env, self);

    aloL_checktag(env, 0, ALO_TSTR);
    GStr* name = v_as_str(*api_stack(env, 0));
    a_bool load = aloL_optbool(env, 1, true);

    alo_settop(env, 1);

    Value v_mod;
    if (!ai_table_gets(env, cache, name, &v_mod)) {
        v_set(env, api_incr_stack(env), v_mod);
        return 1;
    }

    if (load) {
        load_module(env, self, name);
        alo_push(env, 0);
        alo_call(env, 2, 1);

        if (alo_isnil(env, -1)) {
            alo_pushbool(env, true);
        }
        else if (!alo_tobool(env, -1)) {
            return 1;
        }
        v_mod = *api_stack(env, -1);
        ai_table_set(env, cache, v_of_str(name), v_mod);
    }
    else {
        alo_pushbool(env, false);
    }

    return 1;
}

void aloopen_load(a_henv env) {
    static char const* alib_path[] = { ALIB_PATH_DEFAULT };
    static char const* clib_path[] = { CLIB_PATH_DEFAULT };

    static a_cfun const loaders[] = {
        loader_clib,
        loader_alib
    };

    static aloL_Entry const bindings[] = {
        { "load", null },
        { LOADED_FIELD_NAME, null },
        { LOADER_FIEND_NAME, null },
        { ALIB_PATH_FIELD_NAME, null },
        { CLIB_PATH_FIEND_NAME, null },
        { CLIB_CACHE_FIELD_NAME, null }
    };

    alo_newmod(env, 0);
    aloL_putalls(env, -1, bindings);

    alo_push(env, -1);
    alo_newcfun(env, load_load, 1);
    aloL_puts(env, -2, "load");

    alo_newlist(env, 0);
    for (a_u32 i = 0; i < sizeof(loaders) / sizeof(a_cfun); ++i) {
        alo_push(env, -2);
        alo_newcfun(env, loaders[i], 1);
        alo_put(env, -2);
    }
    aloL_puts(env, -2, LOADER_FIEND_NAME);

    alo_newlist(env, 0);
    for (a_u32 i = 0; i < sizeof(alib_path) / sizeof(char const*); ++i) {
        alo_pushntstr(env, alib_path[i]);
        alo_put(env, -2);
    }
    aloL_puts(env, -2, ALIB_PATH_FIELD_NAME);

    alo_newlist(env, 0);
    for (a_u32 i = 0; i < sizeof(clib_path) / sizeof(char const*); ++i) {
        alo_pushntstr(env, clib_path[i]);
        alo_put(env, -2);
    }
    aloL_puts(env, -2, CLIB_PATH_FIEND_NAME);

    alo_newtable(env, 0);
    aloL_puts(env, -2, LOADED_FIELD_NAME);

    alo_newtable(env, 0);
    aloL_puts(env, -2, CLIB_CACHE_FIELD_NAME);
}