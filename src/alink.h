/**
 *@file alink.h
 */

#ifndef alink_h_
#define alink_h_

#include "abuf.h"

#define LHEAD_NAME _lhead
#define LHEAD_DEF LHead LHEAD_NAME
#define LHEAD_REF LHEAD_NAME

#define LINK_NAME _link
#define LINK_DEF Link LINK_NAME
#define LINK_REF LINK_NAME

typedef struct {
	a_u32 _first;
	a_u32 _last;
} LHead;

typedef struct {
	a_x32 _prev;
	a_x32 _next;
} Link;

#define ai_link_new() (new(Link) { nil, nil })

always_inline a_isize ai_link__diff(void* p, void* q, a_usize s) {
	a_byte (*const p_sized)[s] = p;
	a_byte (*const q_sized)[s] = q;
	return p_sized - q_sized;
}

always_inline a_x32 ai_link__wrap(void* b, void* n, a_usize s) {
	if (n == null) return nil;
	return wrap(ai_link__diff(n, b, s));
}

always_inline void* ai_link__unwrap(void* b, a_x32 d, a_usize s) {
	if (is_nil(d)) return null;
	void* n = b + s * unwrap_unsafe(d);
	assume(n != null);
	return n;
}

#define ai_link_wrap(b,n) ai_link__wrap(b, n, sizeof((b)[0]))
#define ai_link_unwrap(n,d) cast(typeof(n), ai_link__unwrap(n, d, sizeof(*(n))))

#define ai_link__get(n,p) ({ \
	typeof(n) _n1 = (n);          \
    ai_link_unwrap(_n1, _n1->LINK_REF.p); \
})

#define ai_link_first(h) ({ typeof(h) _h = (h); _h->BUF_LEN_REF > 0 ? &_h->BUF_PTR_REF[_h->LHEAD_REF._first] : null; })
#define ai_link_prev(n) ai_link__get(n, _prev)
#define ai_link_next(n) ai_link__get(n, _next)

always_inline void ai_link__move(void* ns, void* nd, a_usize s, a_usize f) {
	Link* restrict ls = cast(Link*, ns + f);
	Link* restrict ld = cast(Link*, nd + f);
	void* np = ai_link__unwrap(ns, ls->_prev, s);
	void* nn = ai_link__unwrap(ns, ls->_next, s);
	if (np != null) {
		Link* lp = cast(Link*, np + f);
		lp->_next = ai_link__wrap(np, nd, s);
	}
	if (nn != null) {
		Link* ln = cast(Link*, nn + f);
		ln->_prev = ai_link__wrap(nn, nd, s);
	}
	*ld = new(Link) {
		._prev = ai_link__wrap(nd, np, s),
		._next = ai_link__wrap(nd, nn, s)
	};
}

always_inline void ai_link__push_last(LHead* h, void* b, void* n, a_usize s, a_usize f, a_bool e) {
	Link* l = cast(Link*, n + f);
	if (!e) {
		void* nt = b + h->_last * s;
		Link* lt = cast(Link*, nt + f);
		lt->_next = ai_link__wrap(nt, n, s);
		*l = new(Link) {
			._prev = ai_link__wrap(n, nt, s),
			._next = nil
		};
		h->_last = ai_link__diff(n, b, s);

	}
	else {
		*l = new(Link) {
			._prev = nil,
			._next = nil
		};
		h->_first = h->_last = ai_link__diff(n, b, s);
	}
}

always_inline void ai_link__insert_after(void* nb, void* n, a_usize s, a_usize f) {
	Link* lb = cast(Link*, nb + f);
	Link* l = cast(Link*, n + f);
	void* nn = ai_link__unwrap(nb, lb->_next, s);
	lb->_next = ai_link__wrap(nb, n, s);
	*l = new(Link) {
		._prev = ai_link__wrap(n, n, s),
		._next = ai_link__wrap(n, nn, s)
	};
	if (nn != null) {
		Link* ln = cast(Link*, nn + f);
		ln->_prev = ai_link__wrap(nn, n, s);
	}
}

#define ai_link_move(ns,nd) ai_link__move(ns, nd, sizeof((ns)[0]), offsetof(typeof((ns)[0]), LINK_NAME))
#define ai_link_push_last(h,n,e) ({ \
    typeof(h) _h = (h);            \
	typeof(_h->BUF_PTR_REF) _b = _h->BUF_PTR_REF; \
	ai_link__push_last(&_h->LHEAD_REF, _b, n, sizeof(_b[0]), offsetof(typeof(_b[0]), LINK_NAME), e); \
})
#define ai_link_insert_after(nb,n) ai_link__insert_after(nb, n, sizeof((nb)[0]), offsetof(typeof((nb)[0]), LINK_NAME))

#endif /* alink_h_ */
