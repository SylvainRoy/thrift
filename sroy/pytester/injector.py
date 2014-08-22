#!/usr/bin/env python

from thrift.protocol.TBinaryProtocol import TBinaryProtocol
from thrift.transport.TSocket import TSocket
from thrift.Thrift import *

trans = TSocket(host='localhost', port=9090)
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



prot.writeSetBegin(TType.I32, 3)
prot.writeI32(0)
prot.writeI32(1)
prot.writeI32(2)
prot.writeSetEnd()

## A message:
#prot.writeMessageBegin("MyMessage", 1, 2)
#..
#prot.writeMessageEnd()

## A map:
# prot.writeMapBegin(TType.STRING, TType.I32, 2)
# prot.writeString("key1")
# prot.writeI32(1234)
# prot.writeString("key2")
# prot.writeI32(5678)
# prot.writeMapEnd()

## A struct
#prot.writeStructBegin("MyStruct")
#prot.writeFieldBegin("field1", TType.I32, 0)
#prot.writeI32(12)
#prot.writeFieldEnd()
#prot.writeFieldBegin("field2", TType.BOOL, 0)
#prot.writeBool(True)
#prot.writeFieldEnd()
#prot.writeFieldStop()
#prot.writeStructEnd()

## A list:
# prot.writeListBegin(TType.I32, 3)
# prot.writeI32(0)
# prot.writeI32(1)
# prot.writeI32(2)
# prot.writeListEnd()
