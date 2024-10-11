/**
 *@file aloadlib.c
 */

#define aloadlib_c_
#define ALO_LIB

#include <stdlib.h>

#include "aarch.h"

#if ALO_OS_WINDOWS
# include <winerror.h>
# include <windows.h>
#elif ALO_OS_POSIX
# include <unistd.h>
# include <dlfcn.h>
# include <errno.h>
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

#define setprogdir(env) quiet(env)

static Impl const lib_impl;

#if ALO_OS_WINDOWS

typedef HMODULE a_hlib;

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

static a_msg lib_open(a_henv env, char const* file, a_hlib* plib) {
    a_hlib lib = LoadLibrary(file);
    if (lib == NULL) return lib_error(env);

    *plib = lib;
    return ALO_SOK;
}

static a_msg lib_load(a_henv env, a_hlib lib, char const* sym, a_cfun* pproc) {
    void* addr = GetProcAddress(lib, sym);
    if (addr == null) return lib_error(env);
    a_cfun proc = cast(a_cfun, addr);
    *pproc = proc;
    return ALO_SOK;
}

static void lib_close(a_hlib lib) {
    FreeLibrary(lib);
}

#undef setprogdir

static void setprogdir(a_henv env) {
    char buff[MAX_PATH + 1];
    char const* path = alo_tostr(env, -1);
    char* ls;

    DWORD n = GetModuleFileNameA(null, buff, sizeof(buff));
    if (n == 0 || n == sizeof(buff) / sizeof(char))
        goto bad_get;

    ls = strrchr(buff, '\\');
    if (ls == null)
        goto bad_get;

    *ls = '\0';
    aloS_replace(env, path, ALO_EXEC_DIR, buff);
    alo_erase(env, -2, 1); /* Erase old path. */
    return;

bad_get:
    aloL_raisef(env, "unable get module file dbg_name");
}

#elif ALO_OS_POSIX

typedef void* a_hlib;

static a_msg lib_error(a_henv env) {
    alo_pushntstr(env, dlerror());
    return ALO_EINVAL;
}

static a_msg lib_open(a_henv env, char const* file, a_hlib* plib) {
    catch (access(file, R_OK)) {
        int err = errno;
        if (err == ENOENT) {
            return ALO_EEMPTY;
        }
        alo_pushntstr(env, strerror(err));
        return ALO_EINVAL;
    }
    a_hlib lib = dlopen(file, RTLD_NOW | RTLD_LOCAL);
    *plib = lib;
    return lib != null ? ALO_SOK : lib_error(env);
}

static a_msg lib_load(a_henv env, a_hlib lib, char const* sym, a_cfun* pproc) {
    void* addr = dlsym(lib, sym);
    if (addr == null) return lib_error(env);
    a_cfun proc = cast(a_cfun, addr);
    *pproc = proc;
    return ALO_SOK;
}

static void lib_close(a_hlib lib) {
    dlclose(lib);
}

#else

typedef int a_hlib; /* Dummy value. */

#define LIB_ERR "dynamic library not enabled"

static a_msg lib_open(a_henv env, char const* file, a_hlib* plib) {
    alo_pushstr(env, LIB_ERR);
    return ALO_EINVAL;
}

static a_msg lib_load(a_henv env, a_hlib lib, char const* func, a_cfun* pproc) {
    alo_pushstr(env, LIB_ERR);
    return ALO_EINVAL;
}

static void lib_close(a_hlib lib) {
    unreachable();
}

#endif

typedef struct GLib {
    GOBJ_STRUCT_HEADER;
    a_hlib lib;
} GLib;

static void lib_drop(Global* gbl, GLib* self) {
    lib_close(self->lib);
    ai_mem_dealloc(gbl, self, sizeof(GLib));
}

static void lib_mark(Global* gbl, unused GLib* self) {
    ai_gc_trace_work(gbl, sizeof(GLib));
}

static a_msg lib_new(a_henv env, char const* file, GLib** plib) {
    GLib* self = ai_mem_alloc(env, sizeof(GLib));
    self->impl = &lib_impl;

    catch (lib_open(env, file, &self->lib), msg) {
        ai_mem_dealloc(G(env), self, sizeof(GLib));
        return msg;
    }

    ai_gc_register_normal(env, self);
    v_set_other(env, api_incr_stack(env), self);

    *plib = self;
    return ALO_SOK;
}

static GLib* lib_cast(Value v) {
    if (!v_is_obj(v))
        return null;
    GObj* obj = v_as_obj(v);
    if (obj->impl != &lib_impl)
        return null;
    return g_as(GLib, obj);
}

static Impl const lib_impl = {
    .tag = ALO_TUSER,
    .flags = IMPL_FLAG_GREEDY_MARK,
    .drop = lib_drop,
    .mark = lib_mark
};

#define LOADED_FIELD_NAME "loaded"
#define LOADER_FIEND_NAME "loaders"
#define CLIB_PATH_FIEND_NAME "cpath"
#define CLIB_CACHE_FIELD_NAME "clibs"
#define CLIB_OPEN_FUNC_PREFIX "aloopen_"
#define ALIB_PATH_FIELD_NAME "path"

#define PATH_PLACEHOLDER "?"

#define CAPTURED_SELF_INDEX ALO_STACK_INDEX_CAPTURE(0)

static GType* check_self(a_henv env, a_ilen id) {
    return v_as_type(api_elem(env, id));
}

/*=========================================================*
 * 'loadlib' function support
 *=========================================================*/

static a_noret bad_state(a_henv env) {
    aloL_raisef(env, "invalid module loader state.");
}

static GTable* check_ccache(a_henv env, GType* self) {
    Value v;
    catch (ai_type_getls(env, self, nt2lstr(CLIB_CACHE_FIELD_NAME), &v) || !v_as_table(v)) {
        bad_state(env);
    }
    return v_as_table(v);
}

static a_msg load_clib(a_henv env, GType* self, char const* file, GLib** plib) {
    GLib* lib;
    GTable* cache = check_ccache(env, self);

    Value* r = ai_table_refls(env, cache, nt2lstr(file));
    if (v_is_nil(*r)) {
        catch (lib_new(env, file, &lib), msg) {
            ai_table_delr(env, cache, r);
            return msg;
        }

        v_set_other(env, r, lib);

        ai_gc_barrier_forward(env, cache, lib);
    }
    else {
        lib = lib_cast(*r);
        if (lib == null) {
            alo_pushfstr(env, "library object expected for file '%s'", file);
            return ALO_EINVAL;
        }
    }

    *plib = lib;
    return ALO_SOK;
}

static a_msg load_cfunc(a_henv env, GType* self, char const* file, char const* func) {
    GLib* lib;
    try (load_clib(env, self, file, &lib));

    a_cfun proc;
    try (lib_load(env, lib->lib, func, &proc));

    alo_newcfun(env, proc, 0);
    return ALO_SOK;
}

/*=========================================================*
 * 'use' function support
 *=========================================================*/

static GList* check_list(a_henv env, GType* self, char const* str) {
    Value v;
    catch (ai_type_getls(env, self, nt2lstr(str), &v) || !v_is_list(v)) {
        bad_state(env);
    }
    return v_as_list(v);
}

static char const* check_path(a_henv env, GList* paths, a_u32 i) {
    Value v = paths->ptr[i];
    if (!v_is_str(v)) aloL_raisef(env, "library path must be string", v_name(env, v));
    return str2ntstr(v_as_str(v));
}

typedef struct {
    char const* file;
    char const* proc;
} CLibPath;

static void resolve_clib_path(a_henv env, char const* base, char const* file, CLibPath* out) {
    /* File format like: X/Y-Z */
    out->file = aloS_replace(env, base, PATH_PLACEHOLDER, file);

    char const* p1 = strrchr(file, *ALO_DIR_SEP) ?: file;
    char const* p2 = strchr(p1, '-');
    out->proc = p2 != null ?
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

    GType* self = check_self(env, CAPTURED_SELF_INDEX);
    GList* paths = check_list(env, self, CLIB_PATH_FIEND_NAME);

    aloL_Buf* buf = aloL_newbuf(env);

    for (a_u32 i = 0; i < paths->len; ++i) {
        char const* base = check_path(env, paths, i);
        CLibPath path;
        resolve_clib_path(env, base, file, &path);

        a_msg msg = load_cfunc(env, self, path.file, path.proc);
        if (msg != ALO_EEMPTY) return check_stat(env, msg);

        alo_pushfstr(env, "\n\tno file '%s'", path.file);
        aloL_bufpush(env, buf);

        alo_settop(env, 2);
    }

    aloL_bufstr(env, buf);
    return 1;
}

static a_msg loader_alib(a_henv env) {
    char const* file = aloL_checkstr(env, 0);
    alo_settop(env, 1);

    GType* self = check_self(env, CAPTURED_SELF_INDEX);
    GList* paths = check_list(env, self, ALIB_PATH_FIELD_NAME);

    aloL_Buf* buf = aloL_newbuf(env);

    for (a_u32 i = 0; i < paths->len; ++i) {
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

static GTable* check_cache(a_henv env, GType* self) {
    Value* pv = ai_type_refls(env, self, nt2lstr(LOADED_FIELD_NAME));

    GTable* cache;
    if (v_is_nil(*pv)) {
        cache = ai_table_new(env);

        v_set_table(env, pv, cache);
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

static void load_module(a_henv env, GType* self, GStr* name) {
    GList* loaders = check_list(env, self, LOADER_FIEND_NAME);
    aloL_Buf* buf = aloL_newbuf(env);

    for (a_u32 i = 0; i < loaders->len; ++i) {
        vm_push_args(env, loaders->ptr[i], v_of_str(name));

        catch (alo_pcall(env, 1, 2, ALO_STACK_INDEX_EMPTY)) {
            load_error(env, -1, str2ntstr(name));
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
    GType* self = check_self(env, CAPTURED_SELF_INDEX);
    GTable* cache = check_cache(env, self);

    GStr* name;
    run {
        char const* load_path = aloL_checkstr(env, 0);
        aloS_replace(env, load_path, ".", ALO_DIR_SEP);
        alo_pop(env, 0);
        name = v_as_str(*api_stack(env, 0));
    }
    a_bool load = aloL_optbool(env, 1, true);

    alo_settop(env, 1);

    Value v_mod;
    if (!ai_table_gets(env, cache, name, &v_mod)) {
        v_set(env, api_incr_stack(env), v_mod);
    }
    else if (load) {
        load_module(env, self, name);
        alo_push(env, 0);
        alo_call(env, 2, 1);

        if (alo_isnil(env, -1)) {
            alo_pushbool(env, true);
        }
        else if (alo_tobool(env, -1)) {
            v_mod = *api_stack(env, -1);
            ai_table_set(env, cache, v_of_str(name), v_mod);
        }
    }
    else {
        alo_pushbool(env, false);
    }

    return 1;
}

#ifndef ALO_PATH_VAR
# define ALO_PATH_VAR "ALO_PATH"
#endif

#ifndef ALO_CPATH_VAR
# define ALO_CPATH_VAR "ALO_CPATH"
#endif

#ifndef ALO_VERSIONED_SUFFIX
# define ALO_VERSIONED_SUFFIX "_"ALO_VERSION_STRING("_")
#endif

static void push_paths(a_henv env, char const* paths, char const* dfl) {
    char const* p = paths;
    char const* q;
    a_bool has_dfl = false;
    while ((q = strchr(p, *ALO_PATH_SEP)) != null) {
        if (q[1] == *ALO_PATH_SEP) {
            assume(dfl != null);
            if (!has_dfl) {
                push_paths(env, dfl, null);
                has_dfl = true;
            }
            p = q + 2;
        }
        else {
            if (p < q) {
                alo_pushstr(env, p, q - p);
                setprogdir(env);
                alo_put(env, -2);
            }
            p = q + 1;
        }
    }
    if (*p != '\0') {
        alo_pushntstr(env, p);
        setprogdir(env);
        alo_put(env, -2);
    }
}

static void build_paths(a_henv env,
                        char const* field,
                        char const* var,
                        char const* spec_var,
                        char const* dfl) {
    char const* paths = getenv(spec_var) ?: getenv(var) ?: dfl;

    alo_newlist(env, 0);
    push_paths(env, paths, dfl);
    aloL_puts(env, -2, field);
}

void aloopen_load(a_henv env) {
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

    alo_NewType info = {
        .name = "load"
    };

    alo_newtype(env, &info);
    aloL_putalls(env, -1, bindings);

    build_paths(env, ALIB_PATH_FIELD_NAME, ALO_PATH_VAR, ALO_PATH_VAR ALO_VERSIONED_SUFFIX, ALO_ALIB_PATH);
    build_paths(env, CLIB_PATH_FIEND_NAME, ALO_CPATH_VAR, ALO_CPATH_VAR ALO_VERSIONED_SUFFIX, ALO_CLIB_PATH);

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

    alo_newtable(env, 0);
    aloL_puts(env, -2, LOADED_FIELD_NAME);

    alo_newtable(env, 0);
    aloL_puts(env, -2, CLIB_CACHE_FIELD_NAME);
}