/**
 ** Intrusive hash map and linked array list template.
 *@file amap.h
 */

#ifndef amap_h_
#define amap_h_

#include "abuf.h"

#define MAP_HMASK_NAME _hmask
#define MAP_HMASK_DEF a_u32 MAP_HMASK_NAME
#define MAP_HMASK_REF MAP_HMASK_NAME

#define HLINK_NEXT_NAME _hnext
#define HLINK_NEXT_DEF a_x32 HLINK_NEXT_NAME
#define HLINK_NEXT_REF HLINK_NEXT_NAME

#define LINK_NEXT_NAME _lnext
#define LINK_PREV_NAME _lprev

#define LIST_FIRST_NAME _lfirst
#define LIST_LAST_NAME _llast

#define LIST_LINK_DEF a_u32 LIST_FIRST_NAME; a_u32 LIST_LAST_NAME
#define NODE_LINK_DEF a_x32 LINK_PREV_NAME; a_x32 LINK_NEXT_NAME

#define list_first(m) list_get(m, LIST_FIRST_NAME)
#define list_last(h) list_get(m, LIST_LAST_NAME)

#define link_get(b,f) ({                            \
    typedef typeof(b) restrict _node1_t;            \
	_node1_t _n1 = b;                               \
	is_nil(_n1->f) ? null : ({                      \
		_node1_t _n2 = _n1 + unwrap(_n1->f);        \
        assume(_n2 != null);                        \
		_n2;                                        \
	});                                             \
})

#define link_set(b,f,n) ({                          \
    typedef typeof(b) restrict _node1_t;            \
	_node1_t _n1 = b, _n2 = n;                      \
	_n1->f = _n2 != null ? wrap(_n2 - _n1) : nil;   \
})

#define list_get(m,f) ({ typeof(m) _m1 = m; _m1->BUF_PTR_REF + _m1->f; })

#define list_set(m,f,n) ({                          \
    typeof(m) _m1 = m;                              \
	typeof(n) restrict _n1 = n;                     \
	assume(_n1 == null || (                         \
		_n1 >= _m1->BUF_PTR_REF &&                  \
		_n1 <= _m1->BUF_PTR_REF                     \
				+ _m1->MAP_HMASK_REF));             \
	_m1->f = _n1 - _m1->BUF_PTR_REF;                \
})

#define link_prev(n) link_get(n, LINK_PREV_NAME)
#define link_next(n) link_get(n, LINK_NEXT_NAME)

#define link_move_template(ns,nd,P,N) ({    \
	typedef typeof(ns) restrict _node_t;    \
	_node_t _ns = ns, _nd = nd;             \
	_node_t _np = link_get(_ns,P);          \
	_node_t _nn = link_get(_ns,N);          \
	if (_np != null) {                      \
        _np->N = wrap(_nd - _np);           \
        _nd->P = wrap(_np - _nd);           \
    }                                       \
	else {                                  \
    	_nd->P = nil;                       \
	}                                       \
	if (_nn != null) {                      \
        _nn->P = wrap(_nd - _nn);           \
		_nd->N = wrap(_nn - _nd);           \
	}                                       \
	else {                                  \
    	_nd->N = nil;                       \
	}                                       \
})

#define link_push_template(m,n,F,L,P,N) ({          \
	typedef typeof(n) restrict _node_t;             \
    typeof(m) _m = m;                               \
	typeof(_m->BUF_PTR_REF) _b = _m->BUF_PTR_REF;   \
	_node_t _n = (n);                               \
	if (_m->BUF_LEN_REF > 0) {                      \
		_node_t _nt = &_b[_m->L];                   \
		_nt->N = wrap(_n - _nt);                    \
		_n->P = wrap(_nt - _n);                     \
		_m->L = _n - _b;                            \
	}                                               \
	else {                                          \
		_n->P = nil;                                \
		_m->F = _m->L = _n - _b;                    \
	}                                               \
	_n->N = nil;                                    \
})

#define list_pop_template(m,F,P,N) ({               \
    typeof(m) _m = m;                               \
	typeof(_m->BUF_PTR_REF) _b = _m->BUF_PTR_REF;   \
	typedef typeof(_b) restrict _node_t;            \
    unlikely(_m->BUF_LEN_REF == 0) ? null : ({      \
		_node_t _n = &_b[_m->F];                    \
		_node_t _nn = link_get(_n,N);               \
		if (_nn != null) {                          \
        	_nn->P = nil;                           \
			_m->F = _nn - _b;                       \
		}                                           \
		_n;                                         \
	});                                             \
})

#define link_insert_local_template(nb,n,P,N,Lf) \
({                                              \
	typedef typeof(n) restrict _node_t;         \
	_node_t _nb = nb, _n = n;                   \
	_node_t _nn = link_get(_nb,N);              \
	_nb->N = wrap(_n - _nb);                    \
	_n->P = wrap(_nb - _n);                     \
	if (_nn != null) {                          \
		_n->N = wrap(_nn - _n);                 \
		_nn->P = wrap(_n - _nn);                \
	}                                           \
	else {                                      \
		_n->N = nil;                            \
		Lf(_n);                                 \
	}                                           \
})

#define link_remove_local_template(n,P,N,Ff,Lf) \
({                                              \
	typedef typeof(n) restrict _node_t;         \
	_node_t _n = n;                             \
	_node_t _np = link_get(_n,P);               \
	_node_t _nn = link_get(_n,N);               \
	if (_np != null) {                          \
        link_set(_np, N, _nn);                  \
    }                                           \
	else {                                      \
		Ff(_nn);                                \
	}                                           \
	if (_nn != null) {                          \
		link_set(_nn, P, _np);                  \
	}                                           \
	else {                                      \
		Lf(_np);                                \
	}                                           \
})

#define map_hash_first(m,hash) ({                   \
	typeof(m) _m1 = m;                              \
	&_m1->BUF_PTR_REF[(hash) & _m1->MAP_HMASK_REF]; \
})

#define map_find_template(m,hash,test,empty,con) ({         \
	typeof(m) _m = m;                                       \
	typedef typeof(_m->BUF_PTR_REF) restrict _node_t;       \
	if (likely(_m->BUF_LEN_REF > 0)) {                      \
		_node_t _n = map_hash_first(_m, hash);              \
		if (!empty(_n)) {                                   \
			do {                                            \
				if (test(_n)) { con(_n); }                  \
				_n = link_get(_n, HLINK_NEXT_REF);          \
			}                                               \
			while (_n != null);                             \
		}                                                   \
	}                                                       \
	null;                                                   \
})

#define map_init_links_template(m,P,N) ({           \
	typeof(m) _m = m;                               \
	typeof(_m->BUF_PTR_REF) _b = _m->BUF_PTR_REF;   \
	a_u32 _hmask = _m->MAP_HMASK_REF;               \
	for (a_u32 _i = 0; _i <= _hmask; ++_i) {        \
		a_x32 _in = _i < _hmask ? x32c(1) : nil;    \
		a_x32 _ip = _i > 0 ? x32c(-1) : nil;        \
		_b[_i].N = _in;                             \
		_b[_i].P = _ip;                             \
	}                                               \
})

#endif /* amap_h_ */
