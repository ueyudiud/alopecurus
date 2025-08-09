/**
 *@file aiolib.c
 */

#define ALO_LIB
#define aiolib_c_

#include "aarch.h"

#if ALO_OS_WINDOWS || ALO_OS_POSIX
# include <fcntl.h>
# include <sys/stat.h>
# include <sys/types.h>
#endif

#include <stdio.h>

#include "alo.h"
#include "aauxlib.h"
#include "alolib.h"

#include "aobj.h"
#include "amem.h"
#include "agc.h"
#include "aapi.h"

typedef struct GFile GFile;

struct GFile {
    GOBJ_STRUCT_HEADER;
    FILE* handle;
    int (*closer)(FILE*);
};

#if ALO_OS_WINDOWS
# undef S_IFREG
# define S_IFREG _S_IFREG

# undef fileno
# undef stat
# undef fstat
# define fileno _fileno

# if ALO_M64
#  undef ftell
#  undef fseek
#  define stat _stat64
#  define fstat _fstat64
#  define ftell _ftelli64
#  define fseek _fseeki64
# else
#  define stat _stat32
#  define fstat _fstat32
# endif
#elif ALO_OS_POSIX
# if ALO_M64
#  undef stat
#  undef fstat
#  undef ftell
#  undef fseek
#  define stat stat64
#  define fstat fstat64
#  define ftell ftello64
#  define fseek fseeko64
# endif
#else
# define fileno(h) (-1)
# define fstat(f,s) (-1)
# define aloi_stat(f,s) (-1)
struct aloi_stat { };
#endif

static a_isize file_size0(GFile* self) {
    struct stat s;
    int fno = fileno(self->handle);
#if ALO_OS_WINDOWS
    /* Only binary stream support read directly. */
    int mode = _setmode(fno, _O_BINARY);
    _setmode(fno, mode);
    /*  */
    if (!(mode & _O_BINARY)) {
        return -1;
    }
#endif
    /* Get file attribute */
    try (_fstat64(fno, &s));
    /* Test is regular file */
    if (s.st_mode & S_IFREG) {
        return s.st_size;
    }
    return -1;
}

static a_isize file_pos0(GFile* self) {
    return ftell(self->handle);
}

static a_msg file_open0(a_henv env, GFile** pfile) {
    char const* filename = aloL_checkstr(env, 0);
    char const* mode = aloL_optstr(env, 1) ?: "r";

    GFile* self = ai_mem_alloc(env, sizeof(GFile));
    self->klass = &v_as_type(api_elem(env, ALO_STACK_INDEX_CAPTURE(0)))->as_itype->body;
    int error;
#if ALO_OS_WINDOWS
    error = fopen_s(&self->handle, filename, mode);
#else
    int old_error = errno;
    errno = 0;
    self->handle = fopen(filename, mode);
    error = errno;
    errno = old_error;
#endif
    if (self->handle == null) {
        ai_mem_dealloc(G(env), self, sizeof(GFile));
        return aloL_resultcx(env, false, error, "cannot open file");
    }

    self->closer = fclose;

    ai_gc_register_closable(env, self);
    v_set_other(env, api_incr_stack(env), self);

    *pfile = self;
    return ALO_SOK;
}

static void file_mark0(Global* gbl, GFile* self) {
    g_trace(gbl, g_type(gbl, self));
    ai_gc_trace_work(gbl, sizeof(GFile));
}

static int file_close0(unused a_henv env, GFile* self) {
    if (self->closer != null) {
        int result = (*self->closer)(self->handle);
        self->closer = null;
        return result;
    }
    return 0;
}

static void file_drop0(Global* gbl, GFile* self) {
    ai_mem_dealloc(gbl, self, sizeof(GFile));
}

static GFile* check_file(a_henv env, a_ilen id) {
    Value v = api_elem(env, id);
    return g_as(GFile, v_as_ref(v));
}

static GFile* check_open_file(a_henv env, a_ilen id) {
    GFile* self = check_file(env, id);
    if (self->closer == null) {
        if (id >= 0) {
            aloL_argerror(env, id, "file already closed");
        }
        else {
            aloL_raisef(env, "file already closed");
        }
    }
    return self;
}

static a_msg file_close(a_henv env) {
    GFile* self = check_file(env, 0);
    int error = file_close0(env, self);
    return aloL_resultc(env, error == 0, "close file");
}

typedef struct {
    FILE* handle;
    a_isize size;
} Read;

static a_msg file_read_full(void* ctx, void* dst, a_usize len) {
    FILE* handle = ctx;
    a_usize len2 = fread(dst, 1, len, handle);
    return likely(len2 == len) ? ALO_SOK : ALO_EOUTER;
}

#define READ_SIZE 256

static a_msg file_read(a_henv env) {
    GFile* self = check_open_file(env, 0);
    a_int limit = aloL_optint(env, 1, INT32_MAX);

    if (limit == 0) {
        v_set_str(env, api_incr_stack(env), g_str(env, STR_NIL));
        return 1;
    }

    a_isize fs = file_size0(self);
    a_isize fp = file_pos0(self);

    /* If the stream is a regular file, read directly. */
    if (fs >= 0 && fp >= 0) {
        api_check(fp <= fs, "bad file position.");
        GStr* out;

        catch (ai_str_load(env, file_read_full, fs - fp, self->handle, &out), msg) {
            if (msg == ALO_ENOMEM) {
                ai_mem_nomem(env);
            }
            api_check(msg == ALO_EOUTER);
            return aloL_resultc(env, false, "read file");
        }

        v_set_str(env, api_incr_stack(env), out);
        return 1;
    }

    /* Or else, the stream is a device, read amortized. */
    aloL_Buf* buf = aloL_newbuf(env);
    a_usize rs;
    do {
        void* dst = aloL_bufhint(env, buf, READ_SIZE);
        rs = fread(dst, 1, READ_SIZE, self->handle);
        buf->len += rs;
    }
    while (rs == READ_SIZE);

    if (rs == 0) {
        return aloL_resultc(env, false, "read file");
    }
    aloL_bufstr(env, buf);
    return 1;
}

static a_msg file_line(a_henv env) {
    GFile* self = check_open_file(env, 0);
    aloL_Buf* buf = aloL_newbuf(env);

    a_bool end = false;
    int ch;

    do {
        char* dst = aloL_bufhint(env, buf, READ_SIZE);
        a_u32 i;
        for (i = 0; i < READ_SIZE; ++i) {
            ch = fgetc(self->handle);
            if (unlikely(ch == EOF || ch == '\n')) {
                end = true;
                break;
            }
            dst[i] = cast(char, ch);
        }
        buf->len += i;
    }
    while (!end);

    if (ch == '\n' || buf->len > 0) {
        aloL_bufstr(env, buf);
        return 1;
    }
    if (ferror(self->handle)) {
        return aloL_resultc(env, false, "read file");
    }
    alo_pushnil(env);
    return 1;
}

static void check_file_type(a_henv env) {
    static aloL_Entry const bindings[] = {
        { "__close__", file_close },
        { "__look__", null },
        { "close", file_close },
        { "line", file_line },
        { "read", file_read }
    };

    if (alo_isnil(env, ALO_STACK_INDEX_CAPTURE(0))) {
        GType* self = ai_itype_new(env, &(KClose) {
            .tag = ALO_TUSER,
            .name = "file",
            .flags = 0,
            .mark = file_mark0,
            .close = file_close0,
            .drop = file_drop0
        }, sizeof(KClose));

        v_set_type(env, api_wrslot(env, ALO_STACK_INDEX_CAPTURE(0)), self);

        aloL_putalls(env, ALO_STACK_INDEX_CAPTURE(0), bindings);

        alo_push(env, ALO_STACK_INDEX_CAPTURE(0));
        aloL_puts(env, ALO_STACK_INDEX_CAPTURE(0), "__look__");
    }
}

static a_msg io_open(a_henv env) {
    GFile* file;
    check_file_type(env);
    catch (file_open0(env, &file), msg) {
        return msg;
    }
    return 1;
}

void aloopen_io(a_henv env) {
    static aloL_Entry const bindings[] = {
        { "open", null }
    };

    alo_NewType const info = {
        .name = ALO_LIB_IO_NAME,
        .flags = ALO_NEWTYPE_STATIC
    };

    alo_newtype(env, &info);
    aloL_putalls(env, -1, bindings);

    alo_pushnil(env);
    alo_newcfun(env, io_open, 1);
    aloL_puts(env, -2, "open");
}