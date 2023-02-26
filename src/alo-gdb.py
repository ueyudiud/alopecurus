import gdb

Void = gdb.lookup_type('void')
PVoid = Void.pointer()

Char = gdb.lookup_type('char')
PChar = Char.pointer()

AU64 = gdb.lookup_type('a_u64')

AInt = gdb.lookup_type('a_int')
AFloat = gdb.lookup_type('a_float')

GStr = gdb.lookup_type('GStr')
PGStr = GStr.pointer()

GTuple = gdb.lookup_type('GTuple')
PGTuple = GTuple.pointer()

GList = gdb.lookup_type('GList')
PGList = GList.pointer()

GTable = gdb.lookup_type('GTable')
PGTable = GTable.pointer()

GMod = gdb.lookup_type('GMod')
PGMod = GMod.pointer()

def unwrap_address(val):
    u = val['_u']
    m = ~gdb.Value(0).cast(AU64) >> 17
    return (u & m).cast(PVoid)

def gstr_to_pystr(val):
    l = val['_len']
    p = val['_data']
    return p.cast(PChar).string(length=l)

class ValuePrinter:
    def __init__(self, val: gdb.Value):
        self.val = val
        self.tag = ~val['_'] >> 47

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
            len = val['_len']
            return 'str(len=%s, data=%s)' % (str(len), gstr_to_pystr(val))
        elif self.tag == 7:
            val = unwrap_address(self.val).cast(PGTuple).dereference()
            return 'tuple(len=%s)' % str(val['_len'])
        elif self.tag == 8:
            val = unwrap_address(self.val).cast(PGList).dereference()
            return 'list(len=%s)' % str(val['_len'])
        elif self.tag == 9:
            val = unwrap_address(self.val).cast(PGTable).dereference()
            return 'table(len=%s)' % str(val['_len'])
        elif self.tag == 10:
            return 'func(...)'
        elif self.tag == 11:
            val = unwrap_address(self.val).cast(PGMod).dereference()
            name = val['_name'].dereference()
            len = val['_table']['_len']
            return 'mod(name=%s, len=%s)' % (gstr_to_pystr(name), str(len))
        elif self.tag == 12:
            return 'other'
        else:
            return 'float(%s)' % str(self.val['_f'])


class SingletonPrinter:
    def __init__(self, name):
        self.name = name

    def to_string(self):
        return self.name


NilPrinter = SingletonPrinter('nil')

FalsePrinter = SingletonPrinter('false')

TruePrinter = SingletonPrinter('true')

class IntPrinter:
    def __init__(self, val):
        self.val = val

    def to_string(self):
        return 'int(%s)' % str(self.val)

    def children(self):
        return [('', self.val)]

class PtrPrinter:
    def __init__(self, val):
        self.val = val

    def to_string(self):
        return 'ptr(%s)' % str(self.val)

    def children(self):
        return [('', self.val)]

class StrPrinter:
    def __init__(self, val):
        self.length = val['_len']
        self.data = val['_data']

    def to_string(self):
        return 'str'

    def children(self):
        return [
            ('length', self.length),
            ('body', self.data.string(length=self.length))
        ]


def resolve_value(val: gdb.Value):
    tag = ~val['_h'] >> 15
    if tag == 0:
        return NilPrinter
    elif tag == 1:
        return FalsePrinter
    elif tag == 2:
        return TruePrinter
    elif tag == 3:
        return IntPrinter(val['_i'])
    elif tag == 4:
        return PtrPrinter(unwrap_address(val))
    elif tag == 5 or tag == 6:
        return StrPrinter(unwrap_address(val).cast(PGStr).dereference())
    return ValuePrinter(val)

printer = gdb.printing.RegexpCollectionPrettyPrinter('alo')
printer.add_printer('Value', '^Value$', resolve_value)

gdb.printing.register_pretty_printer(gdb.current_objfile(), printer)