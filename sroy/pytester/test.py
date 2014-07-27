#!/usr/bin/env python

from thrift.protocol.TBinaryProtocol import TBinaryProtocol

class DummyTransport:
    b = ""
    def write(self, s):
        self.b += s
    def __repr__(self):
        return repr(self.b)

trans = DummyTransport()
prot = TBinaryProtocol(trans)

#prot.writeI32(-1)
#prot.writeI16(-1)
#prot.writeByte(-1)
#prot.writeI64(-1)
#prot.writeDouble(-1)
#prot.writeString("Ceci n'est pas trop long.")
prot.writeString("a")

print repr(trans)
