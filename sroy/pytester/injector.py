#!/usr/bin/env python

from thrift.protocol.TBinaryProtocol import TBinaryProtocol
from thrift.transport.TSocket import TSocket

trans = TSocket(host='localhost', port=10000)
trans.open()

prot = TBinaryProtocol(trans, strictWrite=False)

#prot.writeMessageBegin("name", 1, 2)
#prot.writeMessageEnd()
#prot.writeBool(True)
#prot.writeByte(0)
#prot.writeI16(0)
#prot.writeI32(0)
#prot.writeI64(0)
#prot.writeString("a")
#prot.writeFieldBegin("name", 1, 2)
#prot.writeMapBegin(1, 2, 3)
#prot.writeListBegin(1, 2)
#prot.writeSetBegin(1, 2)

prot.writeMessageBegin("name", 1, 2)
