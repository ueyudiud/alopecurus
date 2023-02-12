import gdb

Void = gdb.lookup_type('void')
PVoid = Void.pointer()

AU64 = gdb.lookup_type('a_u64')

AInt = gdb.lookup_type('a_int')
AFloat = gdb.lookup_type('a_float')

GStr = gdb.lookup_type('GStr')
PGStr = GStr.pointer()

GTuple = gdb.lookup_type('GTuple')
PGTuple = GTuple.pointer()

def unwrap_address(val):
    return (val['_u'] & (~gdb.Value(0, AU64) >> 17)).cast(PVoid)

####################################
#   typedef union Value {
# 	    a_f64 _f;
# 	    a_u64 _u;
# 	    struct {
# 	    	union {
# 	    		a_u32 _l;
# 	    		a_i32 _i;
# 	    	};
# 	    	a_u32 _h;
# 	    };
#   } Value;
####################################
class ValuePrinter:
    def __init__(self, val: gdb.Value):
        self.val = val
        self.tag = ~val['_h'] >> 15

    def to_string(self):
        if self.tag == 0:
            return 'nil'
        elif self.tag == 1:
            return 'false'
        elif self.tag == 2:
            return 'true'
        elif self.tag == 3:
            return 'int(%s)' % str(self.val['_i'])
        elif self.tag == 4:
            return 'ptr(%s)' % str(unwrap_address(self.val))
        elif self.tag == 5 or self.tag == 6:
            val = unwrap_address(self.val).cast(PGStr).dereference()
            length = val['_len']
            body = val['_body'].string(length=length)
            return 'str(len=%s, body=%s)' % (str(length), body)
        elif self.tag == 7:
            val = unwrap_address(self.val).cast(PGTuple).dereference()
            return 'tuple(len=%s)' % str(val['_len'])
        else:
            return '???'

printer = gdb.printing.RegexpCollectionPrettyPrinter('alo')
printer.add_printer('Value', '^Value$', ValuePrinter)

gdb.printing.register_pretty_printer(gdb.current_objfile(), printer)